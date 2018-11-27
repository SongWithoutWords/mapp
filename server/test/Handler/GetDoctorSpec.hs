{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GetDoctorSpec (spec) where

import TestImport

setupDB :: SIO (YesodExampleData App) ()
setupDB = runDB $ do
  -- Patients
  mapM_ insert_
    [ Patient "Tom" "Cruise" $ Just $ fromGregorian 1961 2 3
    , Patient "May" "West"   $ Just $ fromGregorian 1962 3 4
    , Patient "Spike" "Lee"  $ Just $ fromGregorian 1963 4 5
    , Patient "Mel" "Brooks" $ Just $ fromGregorian 1964 5 6
    ]
  mapM_ insert_
    [ User "tom@cruise.com" "tcruise" "" $ Right $ patientKey 1
    , User "may@west.com" "mwest" ""     $ Right $ patientKey 2
    , User "spike@lee.com" "slee" ""     $ Right $ patientKey 3
    , User "mel@brooks.com" "mbrooks" "" $ Right $ patientKey 4
    ]

  -- Doctors
  mapM_ insert_
    [ Doctor "Brad" "Pitt"
    , Doctor "Jude" "Law"
    , Doctor "Jet" "Li"
    , Doctor "John" "Wayne"
    ]
  mapM_ insert_
    [ User "brad@pitt.com" "bpitt" ""   $ Left $ doctorKey 1
    , User "jude@law.com" "jlaw" ""     $ Left $ doctorKey 2
    , User "jet@li.com" "jli" ""        $ Left $ doctorKey 3
    , User "john@wayne.com" "jwayne" "" $ Left $ doctorKey 4
    ]

  -- Doctor patient relations
  mapM_ insert_
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

      setupDB

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

      setupDB

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

      setupDB

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

      setupDB

      getAuth ("john@wayne.com", "jwayne") $ DoctorR 4
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 4
        , firstName = "John"
        , lastName = "Wayne"
        , patients = []
        , pendingRequests = []
        }

    it "GET /doctors/4 returns 403 without credentials" $ do
      setupDB
      get $ DoctorR 4
      statusIs 403

    it "GET /doctors/4 returns 403 with wrong credentials" $ do
      setupDB
      getAuth ("jet@li.com", "jli") $ DoctorR 4
      statusIs 403

    it "GET /doctors/7 returns 403 without credentials" $ do
      setupDB
      get $ DoctorR 5
      statusIs 403

    it "GET /doctors/7 returns 403 with wrong credentials" $ do
      setupDB
      -- get $ DoctorR 5
      getAuth ("jet@li.com", "jli") $ DoctorR 7
      statusIs 403
