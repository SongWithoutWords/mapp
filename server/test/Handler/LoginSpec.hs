{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginSpec(spec) where

import TestImport
import TestSetup

spec :: Spec
spec = withApp $ do

  describe "Accounts made with POST /doctors or POST /patients can be accessed with POST /logins" $ do

    it "POST /logins with wrong email returns 403 (forbidden)" $ do
      postPatientBob
      postJson LoginsR $ PostLogin
        { email = "rob@lee.com"
        , password = "rlee"
        }
      statusIs 403

    it "POST /logins with wrong password return 403 (forbidden)" $ do
      postPatientBob
      postJson LoginsR $ PostLogin
        { email = "boby@lee.com"
        , password = "wrong-password"
        }
      statusIs 403

    it "POST /logins with correct credentials return correct patient info" $ do
      postPatientBob
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

    it "POST /logins with correct credentials return correct doctor info" $ do
      postDoctorJim
      postJson LoginsR $ PostLogin
        { email = "james@hill.com"
        , password = "jhill"
        }
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests = []
        }

