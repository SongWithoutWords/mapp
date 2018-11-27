{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.DosageUnit where

-- import Import
import ClassyPrelude.Yesod

data DosageUnit
  = Gram
  | Liter
  deriving(Eq, Generic, Read, Show)
instance FromJSON DosageUnit
instance ToJSON DosageUnit

derivePersistField "DosageUnit"
