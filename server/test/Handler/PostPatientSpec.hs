{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PostPatientSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns and stores the correct information after POST" $ do

      -- Initial request for this email
      postJson PatientsR $ PostPatient
        { firstName = "Jack"
        , lastName = "Black"
        , email = "jack@black.com"
        , password = "jblack"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Jack"
        , lastName = "Black"
        , dateOfBirth = Nothing
        , doctors = []
        , pendingRequests = []
        }

      -- Attempt to claim this email by a second user
      postJson PatientsR $ PostPatient
        { firstName = "Vince"
        , lastName = "Vaughn"
        , email = "jack@black.com"
        , password = "vvaughn"
        , dateOfBirth = Nothing
        }
      statusIs 400

      muser <- runDB $ getBy (UserEmail "jack@black.com")

      case muser of
        Nothing -> failTest "No entry in User table after POST /patients"
        (Just (Entity id user)) -> do
          assertEq "Email" (xUserEmail user) "jack@black.com"
          assertEq "Data" (xUserData user) (Right $ patientKey 1)

          case xUserData user of
            Left _ -> failTest "Patient incorrectly entered as doctor"
            Right did -> do
              mpatient <- runDB $ getEntity did

              case mpatient of
                Nothing -> failTest "No entry in Patient table after POST /patients"
                (Just (Entity _ patient)) -> do
                  assertEq "Patient" patient Patient
                    { patientFirstName = "Jack"
                    , patientLastName = "Black"
                    , patientDateOfBirth = Nothing
                    }
