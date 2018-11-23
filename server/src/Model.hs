{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.DosageUnit

import Text.Read(read)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


-- Information required to POST /login
data PostLogin = PostLogin
  { email :: Text
  , password :: Text
  } deriving(Generic, Show)
instance FromJSON PostLogin
instance ToJSON PostLogin

-- Information required to create a new doctor account
data PostDoctor = PostDoctor
  { email :: Text
  , password :: Text
  , firstName :: Text
  , lastName :: Text
  } deriving(Generic, Show)
instance FromJSON PostDoctor
instance ToJSON PostDoctor

-- Information required to create a new patient account
data PostPatient = PostPatient
  { email :: Text
  , password :: Text
  , firstName :: Text
  , lastName :: Text
  , dateOfBirth :: Maybe Day
  } deriving(Generic, Show)
instance FromJSON PostPatient
instance ToJSON PostPatient

-- Information returned for use by the doctors' app
data DoctorWithPatients = DoctorWithPatients
  { id :: DoctorId
  , firstName :: Text
  , lastName :: Text
  , patients :: [Entity Patient]
  , pendingRequests :: [PendingRequestForDoctor]
  } deriving(Eq, Generic, Show)
instance FromJSON DoctorWithPatients
instance ToJSON DoctorWithPatients

data PendingRequestForDoctor = PendingRequestForDoctor
  { requestId :: DoctorPatientRequestId
  , patient :: Entity Patient
  } deriving(Eq, Generic, Show)
instance FromJSON PendingRequestForDoctor
instance ToJSON PendingRequestForDoctor

-- Information returned for use by the doctors' app
data PatientWithDoctors = PatientWithDoctors
  { id :: PatientId
  , firstName :: Text
  , lastName :: Text
  , dateOfBirth :: Maybe Day
  , doctors :: [Entity Doctor]
  , pendingRequests :: [PendingRequestForPatient]
  , prescriptions :: [GetPrescription]
  } deriving(Eq, Generic, Show)
instance FromJSON PatientWithDoctors
instance ToJSON PatientWithDoctors

data PendingRequestForPatient = PendingRequestForPatient
  { requestId :: DoctorPatientRequestId
  , doctor :: Entity Doctor
  } deriving(Eq, Generic, Show)
instance FromJSON PendingRequestForPatient
instance ToJSON PendingRequestForPatient

data PostPrescription = PostPrescription
  { doctor :: Maybe DoctorId -- optional because the patient may prescribe to themselves
  , patient :: PatientId
  , medication :: Text
  , dosageUnit :: DosageUnit
  , amountInitial :: Double
  , dosageSchedule :: [PostRecurringDose]
  } deriving(Generic)
instance FromJSON PostPrescription
instance ToJSON PostPrescription

data PostRecurringDose = PostRecurringDose
  { firstDose :: UTCTime
  , minutesBetweenDoses :: Int
  , dosage :: Double
  } deriving(Eq, Generic, Show)
instance FromJSON PostRecurringDose
instance ToJSON PostRecurringDose

data GetPrescription = GetPrescription
  { id :: PrescriptionId
  , doctor :: Maybe DoctorId
  , patient :: PatientId
  , medication :: Text
  , dosageUnit :: DosageUnit
  , amountInitial :: Double
  , dosageSchedule :: [PostRecurringDose]
  , dosesTaken :: [DoseTaken]
  } deriving(Eq, Generic, Show)
instance FromJSON GetPrescription
instance ToJSON GetPrescription

-- Utility functions
doctorKey :: Int -> DoctorId
doctorKey = DoctorKey . fromIntegral

patientKey :: Int -> PatientId
patientKey = PatientKey . fromIntegral

requestKey :: Int -> DoctorPatientRequestId
requestKey = DoctorPatientRequestKey . fromIntegral

doctorPatientRequestKey :: Int -> DoctorPatientRequestId
doctorPatientRequestKey = DoctorPatientRequestKey . fromIntegral

doctorPatientRelationKey :: Int -> DoctorPatientRelationId
doctorPatientRelationKey = DoctorPatientRelationKey . fromIntegral

doctorPatientRequest :: Int -> Int -> DoctorPatientRequest
doctorPatientRequest did pid =
  DoctorPatientRequest (doctorKey did) (patientKey pid)

doctorPatientRelation :: Int -> Int -> DoctorPatientRelation
doctorPatientRelation did pid =
  DoctorPatientRelation (doctorKey did) (patientKey pid)


-- Instances for custom types in the database
instance PersistField (Either DoctorId PatientId) where

  toPersistValue :: Either DoctorId PatientId -> PersistValue
  toPersistValue = PersistText . tshow

  fromPersistValue :: PersistValue -> Either Text (Either DoctorId PatientId)
  fromPersistValue pval = case pval of
    PersistText txt -> Right $ read $ unpack txt
    _ -> Left $ "Could not read (Either DoctorId PatientId) from " ++ tshow pval
