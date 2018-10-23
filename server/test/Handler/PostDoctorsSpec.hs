{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PostDoctorsSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns and stores the correct information after POST" $ do

      -- Initial request for this email
      postJson DoctorsR $ PostDoctor
        { firstName = "James"
        , lastName = "Hill"
        , email = "james@hill.com"
        , password = "jhill"
        }
      jsonResponseIs $ DoctorWithPatients (doctorKey 1) "James" "Hill" [] []

      -- Attempt to claim this email by a second user
      postJson DoctorsR $ PostDoctor
        { firstName = "Jill"
        , lastName = "Stein"
        , email = "james@hill.com"
        , password = "jstein"
        }
      statusIs 400

      muser <- runDB $ getBy (UserEmail "james@hill.com")

      case muser of
        Nothing -> failTest "No entry in User table after Doctor POST"
        (Just (Entity id user)) -> do
          assertEq "Email" (xUserEmail user) "james@hill.com"
          assertEq "Data" (xUserData user) (Left $ doctorKey 1)

          case xUserData user of
            Right _ -> failTest "Doctor incorrectly entered as patient"
            Left did -> do
              mdoctor <- runDB $ getEntity did

              case mdoctor of
                Nothing -> failTest "No entry in Doctor table after Doctor POST"
                (Just (Entity _ doctor)) -> do
                  assertEq "Doctor" doctor Doctor
                    { doctorFirstName = "James"
                    , doctorLastName = "Hill"
                    }

      -- Attempt to create a second doctor with same email should not create a second doctor
      get $ DoctorsR
      jsonResponseIs $ [Entity (doctorKey 1) $ Doctor "James" "Hill"]

