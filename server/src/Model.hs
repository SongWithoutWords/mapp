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


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- Utility functions for database keys
doctorKey :: Int -> DoctorId
doctorKey id = DoctorKey $ fromIntegral id

patientKey :: Int -> PatientId
patientKey id = PatientKey $ fromIntegral id

instance PersistField (Either DoctorId PatientId) where

  toPersistValue :: Either DoctorId PatientId -> PersistValue
  toPersistValue id = PersistText $ tshow id

  fromPersistValue :: PersistValue -> Either Text (Either DoctorId PatientId)
  fromPersistValue pval = case pval of
    PersistText txt -> Right $ read $ unpack txt
    _ -> Left $ "Could not read (Either DoctorId PatientId) from " ++ tshow pval
