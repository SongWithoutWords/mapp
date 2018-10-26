{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.RequestsAndRelationsSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "patients and doctors can connect in the app" $ do

    it "patient accounts can be created with POST /patients" $ do

      -- Create patient account
      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "boby@lee.com"
        , password = "blee"
        , dateOfBirth = Nothing
        }
      jsonResponseIs $ Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing

      -- Verify that the patient account exists
      get $ PatientR 1
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

      -- Verify that the doctor account exists
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests = []
        }

      -- Attempt to create a relationship without a request
      postJson RelationsR $ doctorPatientRelation 1 1
      statusIs 400

      -- Check the doctor account, ensure they do not have the patient
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests = []
        }

      -- Add a request from the patient for the doctor
      postJson RequestsR $ doctorPatientRequest 1 1
      jsonResponseIs $ Entity (doctorPatientRequestKey 1) $ doctorPatientRequest 1 1

      -- Confirm the patient's request
      postJson RelationsR $ doctorPatientRelation 1 1
      jsonResponseIs $ Entity (doctorPatientRelationKey 1) $ doctorPatientRelation 1 1

      -- Get the doctor account, ensure that they have the patient
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = [Entity (patientKey 1) $ Patient "Bobby" "Lee" Nothing]
        , pendingRequests = []
        }

