{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.RequestsAndRelationsSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "patients and doctors can connect in the app" $ do

    it "patient accounts can be created with POST /patients" $ do

      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing


