{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GetDoctorSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns a 200 when the doctor exists" $ do

      james <- runDB $ insertEntity $ Doctor "James" "Hill" []
      adam <- runDB $ insertEntity $ Doctor "Adam" "Smith" []
      johan <- runDB $ insertEntity $ Doctor "Johan" "Von-Doyle" []
      alfred <- runDB $ insertEntity $ Doctor "Alfred" "Hitchcock" []

      -- Actual http get request
      get $ DoctorR 1
      jsonResponseIs james

      get $ DoctorR 2
      jsonResponseIs adam

      get $ DoctorR 3
      jsonResponseIs johan

      get $ DoctorR 4
      jsonResponseIs alfred


  describe "invalid requests" $ do
    it "it returns a 404 when the doctor does not exist" $ do

      get $ DoctorR 5
      statusIs 404 -- not found

      get $ DoctorR 7
      statusIs 404 -- not found
