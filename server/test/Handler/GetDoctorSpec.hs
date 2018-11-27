{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GetDoctorSpec (spec) where

import TestImport

setup :: SIO (YesodExampleData App) ()
setup = do

  _ <- mapM (postJson PatientsR)
    [ PostPatient
      { firstName = "Tom"
      , lastName = "Cruise"
      , email = "tom@cruise.com"
      , password = "tcruise"
      , dateOfBirth = Just $ fromGregorian 1961 2 3
      }
    , PostPatient
      { firstName = "May"
      , lastName = "West"
      , email = "may@west.com"
      , password = "mwest"
      , dateOfBirth = Just $ fromGregorian 1962 3 4
      }
    , PostPatient
      { firstName = "Spike"
      , lastName = "Lee"
      , email = "spike@lee.com"
      , password = "slee"
      , dateOfBirth = Just $ fromGregorian 1963 4 5
      }
    , PostPatient
      { firstName = "Mel"
      , lastName = "Brooks"
      , email = "mel@brooks.com"
      , password = "mbrooks"
      , dateOfBirth = Just $ fromGregorian 1964 5 6
      }
    ]

  _ <- mapM (postJson DoctorsR)
    [ PostDoctor
      { firstName = "Brad"
      , lastName = "Pitt"
      , email = "brad@pitt.com"
      , password = "bpitt"
      }
    , PostDoctor
      { firstName = "Jude"
      , lastName = "Law"
      , email = "jude@law.com"
      , password = "jlaw"
      }
    , PostDoctor
      { firstName = "Jet"
      , lastName = "Li"
      , email = "jet@li.com"
      , password = "jli"
      }
    , PostDoctor
      { firstName = "John"
      , lastName = "Wayne"
      , email = "john@wayne.com"
      , password = "jwayne"
      }
    ]

  -- Doctor patient relations
  runDB $ mapM_ insert_
    [ doctorPatientRelation 1 1
    , doctorPatientRelation 1 2
    , doctorPatientRelation 1 4

    , doctorPatientRelation 2 3

    , doctorPatientRelation 3 1
    , doctorPatientRelation 3 2
    , doctorPatientRelation 3 3
    , doctorPatientRelation 3 4
    ]

spec :: Spec
spec = withApp $ do

  describe "GET /doctors/id" $ do

    it "GET /doctors/1 returns 200 and correct data with correct credentials" $ do

      setup

      getAuth ("brad@pitt.com", "bpitt") $ DoctorR 1
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

    it "GET /doctors/2 returns 200 and correct data with correct credentials" $ do

      setup

      getAuth ("jude@law.com", "jlaw") $ DoctorR 2
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

    it "GET /doctors/3 returns 200 and correct data with correct credentials" $ do

      setup

      getAuth ("jet@li.com", "jli") $ DoctorR 3
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

    it "GET /doctors/4 returns 200 and correct data with correct credentials" $ do

      setup

      getAuth ("john@wayne.com", "jwayne") $ DoctorR 4
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 4
        , firstName = "John"
        , lastName = "Wayne"
        , patients = []
        , pendingRequests = []
        }

    it "GET /doctors/4 returns 403 without credentials" $ do
      setup
      get $ DoctorR 4
      statusIs 403

    it "GET /doctors/4 returns 403 with wrong credentials" $ do
      setup
      getAuth ("jet@li.com", "jli") $ DoctorR 4
      statusIs 403

    it "GET /doctors/7 returns 403 without credentials" $ do
      setup
      get $ DoctorR 5
      statusIs 403

    it "GET /doctors/7 returns 403 with wrong credentials" $ do
      setup
      -- get $ DoctorR 5
      getAuth ("jet@li.com", "jli") $ DoctorR 7
      statusIs 403
