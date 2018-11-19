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
  let pids = doctorPatientRelationPatient . entityVal <$> relations
  patients' <- runDB $ mapMaybeM getEntity pids

  requests <- runDB $ selectList [DoctorPatientRequestDoctor ==. did] []
  let rids = doctorPatientRequestPatient . entityVal <$> requests
  pendingRequests' <- runDB $ mapMaybeM getEntity rids

  return $ DoctorWithPatients
    { id = did
    , firstName = fn
    , lastName = ln
    , patients = patients'
    , pendingRequests = pendingRequests'
    }

getPatientWithDoctors :: PatientId -> Handler PatientWithDoctors
getPatientWithDoctors pid = do
  Patient fn ln bd <- runDB $ get404 pid

  relations <- runDB $ selectList [DoctorPatientRelationPatient ==. pid] []
  let dids = doctorPatientRelationDoctor . entityVal <$> relations
  doctors' <- runDB $ mapMaybeM getEntity dids

  requests <- runDB $ selectList [DoctorPatientRequestPatient ==. pid] []
  let rids = doctorPatientRequestDoctor . entityVal <$> requests
  pendingRequests' <- runDB $ mapMaybeM getEntity rids

  prescriptions <- runDB $ selectList [PrescriptionPatient ==. pid] []
  prescriptions' <- mapM mapPrescription prescriptions

  return $ PatientWithDoctors
    { id = pid
    , firstName = fn
    , lastName = ln
    , dateOfBirth = bd
    , doctors = doctors'
    , pendingRequests = pendingRequests'
    , prescriptions = prescriptions'
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

getPrescription :: PrescriptionId -> Handler GetPrescription
getPrescription = dbLookup404 >=> mapPrescription
  -- Prescription did pid med unit amount <- runDB $ get404 prescriptionId

mapPrescription :: Entity Prescription -> Handler GetPrescription
mapPrescription (Entity prescriptionId (Prescription did pid med unit amount)) = do
  schedule <- runDB $ selectList [RecurringDosePrescription ==. prescriptionId] []
  let schedule' = mapRecurringDose . entityVal <$> schedule

  dosesTaken <- runDB $ selectList [DoseTakenPrescription ==. prescriptionId] []
  let dosesTaken' = entityVal <$> dosesTaken

  pure $ GetPrescription prescriptionId did pid med unit amount schedule' dosesTaken'

    where
      mapRecurringDose :: RecurringDose -> PostRecurringDose
      mapRecurringDose (RecurringDose _ first minutesBetween dosage) =
          PostRecurringDose first minutesBetween dosage

postPrescriptionsR :: Handler Value
postPrescriptionsR = do
  PostPrescription did pid med unit amount schedule <- requireJsonBody

  prescriptionId <- runDB $ insert $ Prescription did pid med unit amount

  _ <- runDB $ mapM (insert . mapRecurringDose prescriptionId) schedule

  getPrescription prescriptionId >>= returnJson

    where
      mapRecurringDose :: PrescriptionId -> PostRecurringDose -> RecurringDose
      mapRecurringDose pid (PostRecurringDose first minutesBetween dosage) =
        RecurringDose pid first minutesBetween dosage

postDosesTakenR :: Handler Value
postDosesTakenR = do
  d::DoseTaken <- requireJsonBody
  runDB (insertEntity d) >>= returnJson


