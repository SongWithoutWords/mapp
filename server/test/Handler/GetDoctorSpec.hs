{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GetDoctorSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns a 200 when the doctor exists" $ do


      -- Patients
      tom <- runDB $ insertEntity $
        Patient "Tom" "Cruise" $ fromGregorian 1960 1 2

      may <- runDB $ insertEntity
        $ Patient "May" "West" $ fromGregorian 1961 2 3

      spike <- runDB $ insertEntity $
        Patient "Spike" "Lee" $ fromGregorian 1963 4 5

      mel <- runDB $ insertEntity $
        Patient "Mel" "Brooks" $ fromGregorian 1964 5 6

      -- Doctors
      _ <- runDB $ insertEntity
        $ Doctor "Brad" "Pitt" $ patientKey <$> [1, 2, 4]

      _ <- runDB $ insertEntity
        $ Doctor "Jude" "Law" $ patientKey <$> [3]

      _ <- runDB $ insertEntity
        $ Doctor "Jet" "Li" $ patientKey <$> [1, 2, 3, 4]

      _ <- runDB $ insertEntity
        $ Doctor "John" "Wayne" []

      -- Actual http get request
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "Brad"
        , lastName = "Pitt"
        , patients = [tom, may, mel]
        }

      get $ DoctorR 2
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 2
        , firstName = "Jude"
        , lastName = "Law"
        , patients = [spike]
        }

      get $ DoctorR 3
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 3
        , firstName = "Jet"
        , lastName = "Li"
        , patients = [tom, may, spike, mel]
        }

      get $ DoctorR 4
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 4
        , firstName = "John"
        , lastName = "Wayne"
        , patients = []
        }


  describe "invalid requests" $ do
    it "it returns a 404 when the doctor does not exist" $ do

      get $ DoctorR 5
      statusIs 404 -- not found

      get $ DoctorR 7
      statusIs 404 -- not found
