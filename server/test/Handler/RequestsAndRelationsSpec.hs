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
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors = []
        , pendingRequests = []
        , prescriptions = []
        }

      -- Verify that the patient account exists
      get $ PatientR 1
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors = []
        , pendingRequests = []
        , prescriptions = []
        }

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
      jsonResponseIs $ Entity (requestKey 1) $ doctorPatientRequest 1 1

      -- Ensure that the doctor has the request
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests =
          [ PendingRequestForDoctor
            { requestId = requestKey 1
            , patient = Entity (patientKey 1) $ Patient
              { patientFirstName = "Bobby"
              , patientLastName = "Lee"
              , patientDateOfBirth = Nothing
              }
            }
          ]
        }

      -- Ensure that the patient has the request
      get $ PatientR 1
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors = []
        , pendingRequests =
          [ PendingRequestForPatient
            { requestId = requestKey 1
            , doctor = Entity (doctorKey 1) $ Doctor
              { doctorFirstName = "James"
              , doctorLastName = "Hill"
              }
            }
          ]
        , prescriptions = []
        }

      -- Confirm the patient's request
      postJson RelationsR $ doctorPatientRelation 1 1
      jsonResponseIs $ Entity (relationKey 1) $ doctorPatientRelation 1 1

      -- Ensure that the doctor has the patient
      get $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients =
          [ DoctorViewOfPatient
              { id = patientKey 1
              , firstName = "Bobby"
              , lastName = "Lee"
              , dateOfBirth = Nothing
              , prescriptions = []
              }
          ]
        , pendingRequests = []
        }

      -- Ensure that the patient has the doctor
      get $ PatientR 1
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors =
          [ Entity (doctorKey 1) $ Doctor
              { doctorFirstName = "James"
              , doctorLastName = "Hill"
              }
          ]
        , pendingRequests = []
        , prescriptions = []
        }
