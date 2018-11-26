{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSetup where

import TestImport

postPatientBob :: SIO (YesodExampleData App) ()
postPatientBob = do
  postJson PatientsR $ PostPatient
    { firstName = "Bobby"
    , lastName = "Lee"
    , email = "boby@lee.com"
    , password = "blee"
    , dateOfBirth = Nothing
    }

bobCreds :: (Text, Text)
bobCreds = ("bobby@lee.com", "blee")

postDoctorJim :: SIO (YesodExampleData App) ()
postDoctorJim = do
  postJson DoctorsR $ PostDoctor
    { firstName = "James"
    , lastName = "Hill"
    , email = "james@hill.com"
    , password = "jhill"
    }

jimCreds :: (Text, Text)
jimCreds = ("bobby@lee.com", "blee")

