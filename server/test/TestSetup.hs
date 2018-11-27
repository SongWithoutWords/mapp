{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSetup where

import Data.Time

import Model.DosageUnit
import TestImport

timeFromString :: String -> UTCTime
timeFromString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %l:%M"

postPatientBobbyLee :: SIO (YesodExampleData App) ()
postPatientBobbyLee = postJson PatientsR $ PostPatient
  { firstName = "Bobby"
  , lastName = "Lee"
  , email = "bobby@lee.com"
  , password = "blee"
  , dateOfBirth = Nothing
  }

bobbyLeeCreds :: (Text, Text)
bobbyLeeCreds = ("bobby@lee.com", "blee")

postPatientJackBlack :: SIO (YesodExampleData App) ()
postPatientJackBlack = postJson PatientsR $ PostPatient
  { firstName = "Jack"
  , lastName = "Black"
  , email = "jack@black.com"
  , password = "jblack"
  , dateOfBirth = Nothing
  }

jackBlackCreds :: (Text, Text)
jackBlackCreds = ("jack@black.com", "jblack")

postDoctorJamesHill :: SIO (YesodExampleData App) ()
postDoctorJamesHill = postJson DoctorsR $ PostDoctor
  { firstName = "James"
  , lastName = "Hill"
  , email = "james@hill.com"
  , password = "jhill"
  }

jamesHillCreds :: (Text, Text)
jamesHillCreds = ("james@hill.com", "jhill")

makeRelation :: DoctorId -> (Text, Text) -> PatientId -> (Text, Text) -> SIO (YesodExampleData App) ()
makeRelation did dCreds pid pCreds = do
  postJsonAuth pCreds RequestsR $ DoctorPatientRequest did pid
  postJsonAuth dCreds RelationsR $ DoctorPatientRelation did pid

postAmoxicillin :: Maybe DoctorId -> PatientId -> PostPrescription
postAmoxicillin mdid pid = PostPrescription
  { doctor = mdid
  , patient = pid
  , medication = "amoxicillin"
  , dosageUnit = Gram
  , amountInitial = 5
  , dosageSchedule =
    [ PostRecurringDose
      { firstDose = timeFromString "2019-01-01 09:00"
      , minutesBetweenDoses = 24 * 60
      , dosage = 0.5
      }
    ]
  }

getAmoxicillin :: PrescriptionId -> Maybe DoctorId -> PatientId -> [DoseTaken] -> GetPrescription
getAmoxicillin prescriptionId mdid pid dosesTaken = GetPrescription
  { id = prescriptionId
  , doctor = mdid
  , patient = pid
  , medication = "amoxicillin"
  , dosageUnit = Gram
  , amountInitial = 5
  , dosageSchedule =
    [ PostRecurringDose
      { firstDose = timeFromString "2019-01-01 09:00"
      , minutesBetweenDoses = 24 * 60
      , dosage = 0.5
      }
    ]
  , dosesTaken = dosesTaken
  }

