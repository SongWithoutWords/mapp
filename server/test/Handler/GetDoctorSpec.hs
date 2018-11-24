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
        Patient "Tom" "Cruise" $ Just $ fromGregorian 1960 1 2

      may <- runDB $ insertEntity
        $ Patient "May" "West" $ Just $ fromGregorian 1961 2 3

      spike <- runDB $ insertEntity $
        Patient "Spike" "Lee" $ Just $ fromGregorian 1963 4 5

      mel <- runDB $ insertEntity $
        Patient "Mel" "Brooks" $ Just $ fromGregorian 1964 5 6

      -- Doctors
      _ <- runDB $ mapM insert_
        [ Doctor "Brad" "Pitt"
        , Doctor "Jude" "Law"
        , Doctor "Jet" "Li"
        , Doctor "John" "Wayne"
        ]

      -- Doctor patient relations
      _ <- runDB $ mapM insert_
        [ doctorPatientRelation 1 1
        , doctorPatientRelation 1 2
        , doctorPatientRelation 1 4

        , doctorPatientRelation 2 3

        , doctorPatientRelation 3 1
        , doctorPatientRelation 3 2
        , doctorPatientRelation 3 3
        , doctorPatientRelation 3 4
        ]

      -- Actual http get request
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "Brad"
        , lastName = "Pitt"
        , patients = mapPatient <$> [tom, may, mel]
        , pendingRequests = []
        }

      get $ DoctorR 2
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 2
        , firstName = "Jude"
        , lastName = "Law"
        , patients = [mapPatient spike]
        , pendingRequests = []
        }

      get $ DoctorR 3
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 3
        , firstName = "Jet"
        , lastName = "Li"
        , patients = mapPatient <$> [tom, may, spike, mel]
        , pendingRequests = []
        }

      get $ DoctorR 4
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 4
        , firstName = "John"
        , lastName = "Wayne"
        , patients = []
        , pendingRequests = []
        }


  describe "invalid requests" $ do
    it "it returns a 404 when the doctor does not exist" $ do

      get $ DoctorR 5
      statusIs 404 -- not found

      get $ DoctorR 7
      statusIs 404 -- not found


  where
    mapPatient :: Entity Patient -> DoctorViewOfPatient
    mapPatient (Entity id (Patient fn ln bd)) = DoctorViewOfPatient
      { id = id
      , firstName = fn
      , lastName = ln
      , dateOfBirth = bd
      , prescriptions = []
      }
