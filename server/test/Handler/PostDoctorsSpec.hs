{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PostDoctorsSpec(spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do

  describe "valid request" $ do
    it "returns a 200 when the json is correct" $ do

      postJson DoctorsR $ PostDoctor
        { firstName = "James"
        , lastName = "Hill"
        , email = "james@hill.com"
        , password = "jhill"
        }
      statusIs 200
