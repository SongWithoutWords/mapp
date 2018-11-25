{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GetDoctorSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns a 200 when the doctor exists" $ do

      -- Patients
      _ <- runDB $ mapM insert_
        [ Patient "Tom" "Cruise" $ Just $ fromGregorian 1961 2 3
        , Patient "May" "West"   $ Just $ fromGregorian 1962 3 4
        , Patient "Spike" "Lee"  $ Just $ fromGregorian 1963 4 5
        , Patient "Mel" "Brooks" $ Just $ fromGregorian 1964 5 6
        ]

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
        , patients =
          [ DoctorViewOfPatient
            { id = patientKey 1
            , relationId = relationKey 1
            , firstName = "Tom"
            , lastName = "Cruise"
            , dateOfBirth = Just $ fromGregorian 1961 2 3
            , prescriptions = []
            }
          , DoctorViewOfPatient
            { id = patientKey 2
            , relationId = relationKey 2
            , firstName = "May"
            , lastName = "West"
            , dateOfBirth = Just $ fromGregorian 1962 3 4
            , prescriptions = []
            }
          , DoctorViewOfPatient
            { id = patientKey 4
            , relationId = relationKey 3
            , firstName = "Mel"
            , lastName = "Brooks"
            , dateOfBirth = Just $ fromGregorian 1964 5 6
            , prescriptions = []
            }
          ]
        , pendingRequests = []
        }

      get $ DoctorR 2
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 2
        , firstName = "Jude"
        , lastName = "Law"
        , patients =
          [ DoctorViewOfPatient
            { id = patientKey 3
            , relationId = relationKey 4
            , firstName = "Spike"
            , lastName = "Lee"
            , dateOfBirth = Just $ fromGregorian 1963 4 5
            , prescriptions = []
            }
          ]
        , pendingRequests = []
        }

      get $ DoctorR 3
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 3
        , firstName = "Jet"
        , lastName = "Li"
        , patients =
          [ DoctorViewOfPatient
            { id = patientKey 1
            , relationId = relationKey 5
            , firstName = "Tom"
            , lastName = "Cruise"
            , dateOfBirth = Just $ fromGregorian 1961 2 3
            , prescriptions = []
            }
          , DoctorViewOfPatient
            { id = patientKey 2
            , relationId = relationKey 6
            , firstName = "May"
            , lastName = "West"
            , dateOfBirth = Just $ fromGregorian 1962 3 4
            , prescriptions = []
            }
          , DoctorViewOfPatient
            { id = patientKey 3
            , relationId = relationKey 7
            , firstName = "Spike"
            , lastName = "Lee"
            , dateOfBirth = Just $ fromGregorian 1963 4 5
            , prescriptions = []
            }
          , DoctorViewOfPatient
            { id = patientKey 4
            , relationId = relationKey 8
            , firstName = "Mel"
            , lastName = "Brooks"
            , dateOfBirth = Just $ fromGregorian 1964 5 6
            , prescriptions = []
            }
          ]
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


  -- where
  --   mapPatient :: Entity Patient -> DoctorViewOfPatient
  --   mapPatient (Entity id (Patient fn ln bd)) = DoctorViewOfPatient
  --     { id = id
  --     , firstName = fn
  --     , lastName = ln
  --     , dateOfBirth = bd
  --     , prescriptions = []
  --     }
