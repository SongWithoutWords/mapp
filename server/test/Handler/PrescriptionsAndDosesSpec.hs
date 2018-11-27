{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PrescriptionsAndDosesSpec(spec) where

import Model.DosageUnit

import TestImport
import TestSetup

spec :: Spec
spec = withApp $ describe "POST /prescriptions and POST /doses-taken" $ do

  it "patients can prescribe themselves medication" $ do
    postPatientBobbyLee
    postJsonAuth bobbyLeeCreds PrescriptionsR $ postAmoxicillin Nothing (patientKey 1)
    jsonResponseIs $ getAmoxicillin (prescriptionKey 1) Nothing (patientKey 1) []

  it "doctors can prescribe medication for their patients" $ do
    postPatientBobbyLee
    postDoctorJamesHill
    makeRelation (doctorKey 1) jamesHillCreds (patientKey 1) bobbyLeeCreds
    postJsonAuth jamesHillCreds PrescriptionsR $ postAmoxicillin (Just $ doctorKey 1) (patientKey 1)
    jsonResponseIs $ getAmoxicillin (prescriptionKey 1) (Just $ DoctorKey 1) (patientKey 1) []

  it "medication cannot be prescribe without credentials" $ do
    postPatientBobbyLee
    postJson PrescriptionsR $ postAmoxicillin Nothing (patientKey 1)
    statusIs 403

  it "patients cannot prescribe themselves medication with a doctor id" $ do
    postPatientBobbyLee
    postJsonAuth bobbyLeeCreds PrescriptionsR $ postAmoxicillin (Just $ doctorKey 1) (patientKey 1)
    statusIs 403

  it "patients cannot prescribe medication for other patients" $ do
    postPatientBobbyLee
    postPatientJackBlack
    postJsonAuth jackBlackCreds PrescriptionsR $ postAmoxicillin Nothing (patientKey 1)
    statusIs 403

  it "doctors cannot prescribe medication to those who are not their patients" $ do
    postPatientBobbyLee
    postDoctorJamesHill
    postJsonAuth jamesHillCreds PrescriptionsR $ postAmoxicillin (Just $ doctorKey 1) (patientKey 1)
    statusIs 403

  it "patients can indicate that a dose has been taken with POST /doses-taken" $ do

    postPatientBobbyLee
    postJsonAuth bobbyLeeCreds PrescriptionsR $ postAmoxicillin Nothing (patientKey 1)

    let doseTaken = DoseTaken
          { doseTakenPrescription = prescriptionKey 1
          , doseTakenTime = timeFromString "2019-01-01 09:07"
          , doseTakenAmount = 0.5
          }

    postJsonAuth bobbyLeeCreds DosesTakenR $ doseTaken
    jsonResponseIs $ Entity (doseTakenKey 1) doseTaken

    getAuth bobbyLeeCreds $ PatientR 1
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
          , doctor = Nothing
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
          , dosesTaken = [doseTaken]
          }
        ]
      }

