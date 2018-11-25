{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Handlers where

import Import
import Control.Monad.Extra(mapMaybeM)

dbLookup key = runDB $ getEntity key

dbLookup404 key = do
  maybeValue <- dbLookup key
  case maybeValue of
    Just value -> pure value
    Nothing -> notFound

getDoctorWithPatients :: DoctorId -> Handler DoctorWithPatients
getDoctorWithPatients did = do
  Doctor fn ln <- runDB $ get404 did

  relations <- runDB $ selectList [DoctorPatientRelationDoctor ==. did] []
  patients <- mapMaybeM getDoctorViewOfPatient relations

  requests <- runDB $ selectList [DoctorPatientRequestDoctor ==. did] []
  requests' <- mapMaybeM getRequestForDoctor requests

  return $ DoctorWithPatients
    { id = did
    , firstName = fn
    , lastName = ln
    , patients = patients
    , pendingRequests = requests'
    }

  where
    getRequestForDoctor
      :: Entity DoctorPatientRequest
      -> Handler (Maybe PendingRequestForDoctor)
    getRequestForDoctor (Entity key (DoctorPatientRequest _ pid)) = do
      mpatient <- runDB $ getEntity pid
      pure $ case mpatient of
        Nothing -> Nothing
        Just patient -> Just $ PendingRequestForDoctor key patient

    getDoctorViewOfPatient
      :: Entity DoctorPatientRelation
      -> Handler (Maybe DoctorViewOfPatient)
    getDoctorViewOfPatient (Entity rid (DoctorPatientRelation _ pid)) = do
      mpatient <- runDB $ get pid
      case mpatient of
        Nothing -> pure Nothing
        Just (Patient fn ln bd) -> do
          prescriptions' <- getPrescriptionsForPatient pid
          pure $ Just $ DoctorViewOfPatient
            { id = pid
            , relationId = rid
            , firstName = fn
            , lastName = ln
            , dateOfBirth = bd
            , prescriptions = prescriptions'
            }

getPrescriptionsForPatient :: PatientId -> Handler [GetPrescription]
getPrescriptionsForPatient pid = do
  prescriptions <- runDB $ selectList [PrescriptionPatient ==. pid] []
  mapM mapPrescription prescriptions

getPatientWithDoctors :: PatientId -> Handler PatientWithDoctors
getPatientWithDoctors pid = do
  Patient fn ln bd <- runDB $ get404 pid

  relations <- runDB $ selectList [DoctorPatientRelationPatient ==. pid] []
  doctors <- mapMaybeM getPatientViewOfDoctor relations

  requests <- runDB $ selectList [DoctorPatientRequestPatient ==. pid] []
  requests' <- mapMaybeM getRequestForPatient requests

  prescriptions' <- getPrescriptionsForPatient pid

  return $ PatientWithDoctors
    { id = pid
    , firstName = fn
    , lastName = ln
    , dateOfBirth = bd
    , doctors = doctors
    , pendingRequests = requests'
    , prescriptions = prescriptions'
    }

  where
    getRequestForPatient
      :: Entity DoctorPatientRequest
      -> Handler (Maybe PendingRequestForPatient)
    getRequestForPatient (Entity key (DoctorPatientRequest did _)) = do
      mdoctor <- runDB $ getEntity did
      pure $ case mdoctor of
        Nothing -> Nothing
        Just doctor -> Just $ PendingRequestForPatient key doctor

    getPatientViewOfDoctor
      :: Entity DoctorPatientRelation
      -> Handler (Maybe PatientViewOfDoctor)
    getPatientViewOfDoctor (Entity rid (DoctorPatientRelation did _)) = do
      mdoctor <- runDB $ get did
      case mdoctor of
        Nothing -> pure Nothing
        Just (Doctor fn ln) -> do
          prescriptions' <- getPrescriptionsForPatient pid
          pure $ Just $ PatientViewOfDoctor
            { id = did
            , relationId = rid
            , firstName = fn
            , lastName = ln
            }

postLoginsR :: Handler Value
postLoginsR = do
  PostLogin emailReceived passwordReceived <- requireJsonBody
  maybeUser <- runDB $ getBy $ UniqueUser emailReceived
  case maybeUser of
    Nothing -> invalidArgs ["No account for this email address"]
    Just (Entity _ (User _ password' _ doctorOrPatientId')) ->
      if passwordReceived /= password'
        then permissionDenied "Incorrect password"
        else case doctorOrPatientId' of
          Left did -> getDoctorWithPatients did >>= returnJson
          Right pid -> getPatientWithDoctors pid >>= returnJson

getDoctorR :: Int -> Handler Value
getDoctorR = getDoctorWithPatients . doctorKey >=> returnJson

getPatientR :: Int -> Handler Value
getPatientR = getPatientWithDoctors . patientKey >=> returnJson

getDoctorsR :: Handler Value
getDoctorsR = do
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs

postDoctorsR :: Handler Value
postDoctorsR = do
  PostDoctor email' password' firstName' lastName' <- requireJsonBody
  doctorId <- runDB $ insert Doctor
    { doctorFirstName = firstName'
    , doctorLastName = lastName'
    }
  let user = User email' password' "" (Left doctorId)
  userInserted <- runDB $ insertUniqueEntity user

  case userInserted of
    Nothing -> do
      _ <- runDB $ delete doctorId
      invalidArgs ["Email already in use"]
    Just _ -> getDoctorWithPatients doctorId >>= returnJson

postPatientsR :: Handler Value
postPatientsR =  do
  PostPatient email' password' fn ln bd <- requireJsonBody
  patientId <- runDB $ insert Patient
    { patientFirstName = fn
    , patientLastName = ln
    , patientDateOfBirth = bd
    }

  let user = User email' password' "" (Right patientId)
  userInserted <- runDB $ insertUniqueEntity user
  case userInserted of
    Nothing -> do
      _ <- runDB $ delete patientId
      invalidArgs ["Email already in use"]
    Just _ -> getPatientWithDoctors patientId >>= returnJson

postRequestsR :: Handler Value
postRequestsR =  do
  request :: DoctorPatientRequest <- requireJsonBody
  requestInserted <- runDB $ insertUniqueEntity request
  returnJson requestInserted

deleteRequestR :: Int -> Handler ()
deleteRequestR = runDB . delete . requestKey

postRelationsR :: Handler Value
postRelationsR = do
  relation@(DoctorPatientRelation did pid) <- requireJsonBody

  pendingRequests <- runDB $ selectList
    [ DoctorPatientRequestDoctor ==. did
    , DoctorPatientRequestPatient ==. pid] []

  if null pendingRequests
    then invalidArgs ["No pending request from this patient to this doctor"]
    else do
      _ <- runDB $ mapM (delete . entityKey) pendingRequests
      runDB (insertUniqueEntity relation) >>= returnJson

deleteRelationR :: Int -> Handler ()
deleteRelationR = runDB . delete . relationKey

getPrescription :: PrescriptionId -> Handler GetPrescription
getPrescription = dbLookup404 >=> mapPrescription

mapPrescription :: Entity Prescription -> Handler GetPrescription
mapPrescription (Entity prescriptionId (Prescription did pid med unit amount)) = do
  schedule <- runDB $ selectList [RecurringDosePrescription ==. prescriptionId] []
  let schedule' = recurringDoseToJs . entityVal <$> schedule

  dosesTaken <- runDB $ selectList [DoseTakenPrescription ==. prescriptionId] []
  let dosesTaken' = entityVal <$> dosesTaken

  pure $ GetPrescription prescriptionId did pid med unit amount schedule' dosesTaken'


-- Convert recurring dose from database to json representation
recurringDoseToJs :: RecurringDose -> PostRecurringDose
recurringDoseToJs (RecurringDose _ first minutesBetween dosage) =
    PostRecurringDose first minutesBetween dosage

-- Convert recurring dose from json to database representation
recurringDoseToDb :: PrescriptionId -> PostRecurringDose -> RecurringDose
recurringDoseToDb pid (PostRecurringDose first minutesBetween dosage) =
  RecurringDose pid first minutesBetween dosage

insertSchedule :: PrescriptionId -> [PostRecurringDose] -> Handler ()
insertSchedule pid schedule = runDB $ mapM (insert . recurringDoseToDb pid) schedule >> pure ()

deleteSchedule :: PrescriptionId -> Handler ()
deleteSchedule pid = runDB $ deleteWhere [RecurringDosePrescription ==. pid]

postPrescriptionsR :: Handler Value
postPrescriptionsR = do
  PostPrescription did pid med unit amount schedule <- requireJsonBody

  prescriptionId <- runDB $ insert $ Prescription did pid med unit amount

  insertSchedule prescriptionId schedule

  getPrescription prescriptionId >>= returnJson

patchPrescriptionR :: Int -> Handler Value
patchPrescriptionR id = do
  let prescriptionId = prescriptionKey id
  PostPrescription did pid med unit amount schedule <- requireJsonBody

  runDB $ replace prescriptionId $ Prescription did pid med unit amount
  deleteSchedule prescriptionId
  insertSchedule prescriptionId schedule

  getPrescription prescriptionId >>= returnJson

deletePrescriptionR :: Int -> Handler ()
deletePrescriptionR = runDB . delete . prescriptionKey

postDosesTakenR :: Handler Value
postDosesTakenR = do
  d::DoseTaken <- requireJsonBody
  runDB (insertEntity d) >>= returnJson

