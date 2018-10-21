{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PostDoctorsSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns and stores the correct information after POST" $ do

      postJson DoctorsR $ PostDoctor
        { firstName = "James"
        , lastName = "Hill"
        , email = "james@hill.com"
        , password = "jhill"
        }
      jsonResponseIs $ Entity (doctorKey 1) $ Doctor "James" "Hill" []

      muser <- runDB $ getBy (UserEmail "james@hill.com")

      case muser of
        Nothing -> failTest "Doctor's user credentials not correctly stored after post"
        (Just (Entity id user)) -> do
          assertEq "Email" (xUserEmail user) "james@hill.com"
          assertEq "Data" (xUserData user) (Left $ doctorKey 1)

