{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.DoctorGet(spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns a 200 when the doctor exists" $ do

      runDB $ do
        _ <- insert $ Doctor "James" "Hill" []
        _ <- insert $ Doctor "Adam" "Smith" []
        _ <- insert $ Doctor "Johan" "Von-Doyle" []
        _ <- insert $ Doctor "Alfred" "Hitchcock" []
        pure ()

      -- Actual http get request
      get $ DoctorR 2
      statusIs 200
      bodyContains "Adam"
      bodyContains "Smith"
      bodyNotContains "Alfred"
      bodyNotContains "Hitchcock"

      get $ DoctorR 3
      statusIs 200
      bodyContains "Johan"
      bodyContains "Von-Doyle"
      bodyNotContains "James"
      bodyNotContains "Hill"

  describe "invalid requests" $ do
    it "it returns a 404 when the doctor does not exist" $ do

      get $ DoctorR 5
      statusIs 404 -- not found

      get $ DoctorR 7
      statusIs 404 -- not found
