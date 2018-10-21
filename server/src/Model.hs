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

import ClassyPrelude.Yesod hiding(id)
import Database.Persist.Quasi

import Text.Read(read)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


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
  , dateOfBirth :: Day
  } deriving(Generic, Show)
instance FromJSON PostPatient
instance ToJSON PostPatient

-- Information returned for use by the doctors' app
data DoctorWithPatients = DoctorWithPatients
  { id :: DoctorId
  , firstName :: Text
  , lastName :: Text
  , patients :: [Entity Patient]
  } deriving(Eq, Generic, Show)
instance FromJSON DoctorWithPatients
instance ToJSON DoctorWithPatients


-- Utility functions
doctorKey :: Int -> DoctorId
doctorKey = DoctorKey . fromIntegral

patientKey :: Int -> PatientId
patientKey = PatientKey . fromIntegral

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
