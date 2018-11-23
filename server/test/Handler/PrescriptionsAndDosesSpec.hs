{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PrescriptionsAndDosesSpec(spec) where

-- import Data.UTC
-- import Data.UTCTime
import Data.Time

import TestImport
import Model.DosageUnit

spec :: Spec
spec = withApp $ do

  describe "patients can be prescribed medication within the app" $ do

    it "patients can be prescribed medication within the app" $ do

      -- Create patient account
      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing

      -- Create a doctor account
      postJson DoctorsR $ PostDoctor
        { firstName = "James"
        , lastName = "Hill"
        , email = "james@hill.com"
        , password = "jhill"
        }
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests = []
        }

      let timeFromString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %l:%M"

      -- Add a prescription with POST /prescription
      postJson PrescriptionsR $ PostPrescription
        { doctor = Just $ doctorKey 1
        , patient = patientKey 1
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
      jsonResponseIs $ GetPrescription
        { id = PrescriptionKey 1
        , doctor = Just $ doctorKey 1
        , patient = patientKey 1
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
        , dosesTaken = []
        }

      -- Indicate that a dose has been taken with POST /doses-taken
      postJson DosesTakenR $ DoseTaken
        { doseTakenPrescription = PrescriptionKey 1
        , doseTakenTime = timeFromString "2019-01-01 09:07"
        , doseTakenAmount = 0.5
        }

      -- Get the patient's info and ensure that everything is working
      get $ PatientR 1
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors = []
        , pendingRequests = []
        , prescriptions =
          [ GetPrescription
            { id = PrescriptionKey 1
            , doctor = Just $ doctorKey 1
            , patient = patientKey 1
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
            , dosesTaken =
              [ DoseTaken
                { doseTakenPrescription = PrescriptionKey 1
                , doseTakenTime = timeFromString "2019-01-01 09:07"
                , doseTakenAmount = 0.5
                }
              ]
            }
          ]
        }

