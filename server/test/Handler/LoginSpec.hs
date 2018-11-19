{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "users can create accounts and log back in using their email and password" $ do

    it "logins with unmatched email return invalid args" $ do

      -- Create patient account
      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing

      postJson LoginsR $ PostLogin
        { email = "rob@lee.com"
        , password = "rlee"
        }
      statusIs 400 -- bad arguments

    it "logins with wrong password return permission denied" $ do

      -- Create patient account
      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing

      postJson LoginsR $ PostLogin
        { email = "boby@lee.com"
        , password = "wrong-password"
        }
      statusIs 403 -- bad arguments

    it "patient account logins with correct password and email return correct info" $ do

      -- Create patient account
      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing

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

    it "doctor account logins with correct password and email return correct info" $ do

      -- Create patient account
      postJson DoctorsR $ PostDoctor
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        }
      jsonResponseIs $ Entity (doctorKey 1) $ Doctor "Bobby" "Lee"

      postJson LoginsR $ PostLogin
        { email = "boby@lee.com"
        , password = "blee"
        }
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , patients = []
        , pendingRequests = []
        }

