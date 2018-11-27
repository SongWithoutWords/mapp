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

authenticateHeader :: Handler (Either DoctorId PatientId)
authenticateHeader = do
  mAuth <- lookupBasicAuth
  case mAuth of
    Nothing -> permissionDenied "Must provide email and password in Authorization header"
    Just (emailRec, passwordRec) -> authenticate emailRec passwordRec

authenticate :: Text -> Text -> Handler (Either DoctorId PatientId)
authenticate emailRec passwordRec = do
  maybeUser <- runDB $ getBy $ UniqueUser emailRec
  case maybeUser of
    Nothing -> permissionDenied "No account for this email address"
    Just (Entity _ (User _ userPassword _ doctorOrPatientId')) ->
      if passwordRec /= userPassword
        then permissionDenied "Incorrect password"
        else pure doctorOrPatientId'

userMustBePatient :: PatientId -> PatientId -> Handler ()
userMustBePatient patientRequired patientFound =
  unless (patientFound == patientRequired) $
    permissionDenied "User must be the correct patient"

userMustBeAPatient :: DoctorId -> Handler ()
userMustBeAPatient _ = permissionDenied "User must be a patient"

userMustBeDoctor :: DoctorId -> DoctorId -> Handler ()
userMustBeDoctor doctorRequired doctorFound =
  unless (doctorFound == doctorRequired) $
    permissionDenied "User must be the correct doctor"

userMustBeADoctor :: PatientId -> Handler ()
userMustBeADoctor _ = permissionDenied "User must be a doctor"

userMustBeDoctorOf :: PatientId -> DoctorId -> Handler ()
userMustBeDoctorOf pid did = do
      doctorPatientRelationExists <- runDB $ (> 0) <$> count
        [ DoctorPatientRelationDoctor ==. did
        , DoctorPatientRelationPatient ==. pid]
      unless doctorPatientRelationExists $
        permissionDenied "User must be a doctor of the patient"

authenticateEither :: (DoctorId -> Handler ()) -> (PatientId -> Handler ()) -> Handler ()
authenticateEither fa fb = authenticateHeader >>= either fa fb

authenticateAsPatient :: PatientId -> Handler ()
authenticateAsPatient pid =
  authenticateEither userMustBeAPatient (userMustBePatient pid)

authenticateAsDoctor :: DoctorId -> Handler ()
authenticateAsDoctor did =
  authenticateEither (userMustBeDoctor did) userMustBeADoctor

authenticateAsDoctorOrPatient :: DoctorId -> PatientId -> Handler ()
authenticateAsDoctorOrPatient did pid =
  authenticateEither (userMustBeDoctor did) (userMustBePatient pid)

authenticateAsDoctorOfPatientOrElsePatient :: Maybe DoctorId -> PatientId -> Handler ()
authenticateAsDoctorOfPatientOrElsePatient mdid pid =
  case mdid of
    Just did -> authenticateAsDoctor did >> userMustBeDoctorOf pid did
    Nothing -> authenticateAsPatient pid

authenticateAsPatientOrDoctorOf :: PatientId -> Handler ()
authenticateAsPatientOrDoctorOf pid =
  authenticateEither (userMustBeDoctorOf pid) (userMustBePatient pid)

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
  doctorOrPatientId' <- authenticate emailReceived passwordReceived

  case doctorOrPatientId' of
    Left did -> getDoctorWithPatients did >>= returnJson
    Right pid -> getPatientWithDoctors pid >>= returnJson

getDoctorR :: Int -> Handler Value
getDoctorR id = do
  let did = doctorKey id
  authenticateAsDoctor did
  getDoctorWithPatients did >>= returnJson

getPatientR :: Int -> Handler Value
getPatientR id = do
  let pid = patientKey id
  authenticateAsPatient pid
  getPatientWithDoctors pid >>= returnJson

getDoctorsR :: Handler Value
getDoctorsR = do
  docs :: [Entity Doctor] <- runDB $ selectList [] []
  returnJson docs

postDoctorsR :: Handler Value
postDoctorsR = do
  -- No authentication required: creating a new account
  PostDoctor email' password' firstName' lastName' <- requireJsonBody
  doctorId <- runDB $ insert Doctor
    { doctorFirstName = firstName'
    , doctorLastName = lastName'
    }
  let user = User email' password' "" (Left doctorId)
  userInserted <- runDB $ insertUniqueEntity user

  case userInserted of
    Nothing -> do
      runDB $ delete doctorId
      invalidArgs ["Email already in use"]
    Just _ -> getDoctorWithPatients doctorId >>= returnJson

postPatientsR :: Handler Value
postPatientsR =  do
  -- No authentication required: creating a new account
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
      runDB $ delete patientId
      invalidArgs ["Email already in use"]
    Just _ -> getPatientWithDoctors patientId >>= returnJson

postRequestsR :: Handler Value
postRequestsR =  do
  request@(DoctorPatientRequest _ pid) <- requireJsonBody
  authenticateAsPatient pid
  requestInserted <- runDB $ insertUniqueEntity request
  returnJson requestInserted

deleteRequestR :: Int -> Handler ()
deleteRequestR id = do
  let rid = requestKey id
  mreq <- runDB $ get rid
  case mreq of
    Nothing -> notFound
    Just (DoctorPatientRequest did pid) -> do
      authenticateAsDoctorOrPatient did pid
      runDB $ delete rid

postRelationsR :: Handler Value
postRelationsR = do
  relation@(DoctorPatientRelation did pid) <- requireJsonBody
  authenticateAsDoctor did
  pendingRequests <- runDB $ selectList
    [ DoctorPatientRequestDoctor ==. did
    , DoctorPatientRequestPatient ==. pid] []

  if null pendingRequests
    then invalidArgs ["No pending request from this patient to this doctor"]
    else do
      runDB $ mapM_ (delete . entityKey) pendingRequests
      runDB (insertUniqueEntity relation) >>= returnJson

deleteRelationR :: Int -> Handler ()
deleteRelationR id = do
  let rid = relationKey id
  mrel <- runDB $ get rid
  case mrel of
    Nothing -> notFound
    Just (DoctorPatientRelation did pid) -> do
      authenticateAsDoctorOrPatient did pid
      runDB $ delete rid

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
insertSchedule pid schedule = runDB $ mapM_ (insert_ . recurringDoseToDb pid) schedule

deleteScheduleAndDosesTaken :: PrescriptionId -> Handler ()
deleteScheduleAndDosesTaken pid = do
  runDB $ deleteWhere [RecurringDosePrescription ==. pid]
  runDB $ deleteWhere [DoseTakenPrescription ==. pid]

postPrescriptionsR :: Handler Value
postPrescriptionsR = do
  PostPrescription did pid med unit amount schedule <- requireJsonBody

  authenticateAsDoctorOfPatientOrElsePatient did pid

  prescriptionId <- runDB $ insert $ Prescription did pid med unit amount

  insertSchedule prescriptionId schedule

  getPrescription prescriptionId >>= returnJson

patchPrescriptionR :: Int -> Handler Value
patchPrescriptionR id = do
  let prescriptionId = prescriptionKey id

  mPrescription <- runDB $ get prescriptionId

  case mPrescription of
    Nothing -> notFound
    Just (Prescription did pid _ _ _) -> do
      authenticateAsDoctorOfPatientOrElsePatient did pid

      PatchPrescription med unit amount schedule <- requireJsonBody

      deleteScheduleAndDosesTaken prescriptionId
      runDB $ replace prescriptionId $ Prescription did pid med unit amount
      insertSchedule prescriptionId schedule

      getPrescription prescriptionId >>= returnJson

deletePrescriptionR :: Int -> Handler ()
deletePrescriptionR id = do
  let prescriptionId = prescriptionKey id

  mPrescription <- runDB $ get prescriptionId

  case mPrescription of
    Nothing -> notFound
    Just (Prescription did pid _ _ _) -> do
      authenticateAsDoctorOfPatientOrElsePatient did pid

      deleteScheduleAndDosesTaken prescriptionId
      runDB $ delete prescriptionId

postDosesTakenR :: Handler Value
postDosesTakenR = do
  d@(DoseTaken prescriptionId _ _) <- requireJsonBody
  mPrescription <- runDB $ get prescriptionId
  case mPrescription of
    Nothing -> invalidArgs ["Prescription does not exist"]
    Just (Prescription _ pid _ _ _) -> do
      authenticateAsPatient pid
      runDB (insertEntity d) >>= returnJson
