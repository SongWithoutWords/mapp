{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginSpec(spec) where

import TestImport

postPatient :: SIO (YesodExampleData App) ()
postPatient = do
  postJson PatientsR $ PostPatient
    { firstName = "Bobby"
    , lastName = "Lee"
    , email = "boby@lee.com"
    , password = "blee"
    , dateOfBirth = Nothing
    }
  jsonResponseIs $ PatientWithDoctors
    { id = patientKey 1
    , firstName = "Bobby"
    , lastName = "Lee"
    , dateOfBirth = Nothing
    , doctors = []
    , pendingRequests = []
    , prescriptions = []
    }

postDoctor :: SIO (YesodExampleData App) ()
postDoctor = do
  postJson DoctorsR $ PostDoctor
    { firstName = "Doctor"
    , lastName = "House"
    , email = "doctor@house.com"
    , password = "dhouse"
    }
  jsonResponseIs $ DoctorWithPatients
    { id = doctorKey 1
    , firstName = "Doctor"
    , lastName = "House"
    , patients = []
    , pendingRequests = []
    }

spec :: Spec
spec = withApp $ do

  describe "Accounts made with POST /doctors or POST /patients can be accessed with POST /logins" $ do

    it "POST /logins with wrong email returns 403 (forbidden)" $ do
      postPatient
      postJson LoginsR $ PostLogin
        { email = "rob@lee.com"
        , password = "rlee"
        }
      statusIs 403

    it "logins with wrong password return 403 (forbidden)" $ do
      postPatient
      postJson LoginsR $ PostLogin
        { email = "boby@lee.com"
        , password = "wrong-password"
        }
      statusIs 403

    it "patient account logins with correct credentials return correct info" $ do
      postPatient
      postJson LoginsR $ PostLogin
        { email = "boby@lee.com"
        , password = "blee"
        }
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors = []
        , pendingRequests = []
        , prescriptions = []
        }

    it "doctor account logins with correct credentials return correct info" $ do

      -- Create patient account
      -- postJson DoctorsR $ PostDoctor
      --   { firstName = "Bobby"
      --   , lastName = "Lee"
      --   , email = "boby@lee.com"
      --   , password = "blee"
      --   }
      -- jsonResponseIs $ Entity (doctorKey 1) $ Doctor "Bobby" "Lee"

      postDoctor

      postJson LoginsR $ PostLogin
        { email = "doctor@house.com"
        , password = "dhouse"
        }
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "Doctor"
        , lastName = "House"
        , patients = []
        , pendingRequests = []
        }

