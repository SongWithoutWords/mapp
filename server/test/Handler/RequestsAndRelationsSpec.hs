{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.RequestsAndRelationsSpec(spec) where

import TestImport

spec :: Spec
spec = withApp $ do

  describe "POST /reequests and POST /relations" $ do

    it "patients and doctors can connect in the app" $ do

      -- Create patient account
      postJson PatientsR $ PostPatient
        { firstName = "Bobby"
        , lastName = "Lee"
        , email = "bobby@lee.com"
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

      let bobbyCreds = ("bobby@lee.com", "blee")

      -- Verify that the patient account exists
      getAuth bobbyCreds (PatientR 1)
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

      let jamesCreds = ("james@hill.com", "jhill")

      -- Verify that the doctor account exists
      getAuth jamesCreds $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests = []
        }

      -- Attempt a relation without credentials
      postJson RelationsR $ doctorPatientRelation 1 1
      statusIs 403

      -- Attempt a relation without a request
      postJsonAuth jamesCreds RelationsR $ doctorPatientRelation 1 1
      statusIs 400

      -- Check the doctor account, ensure they do not have the patient
      getAuth jamesCreds $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients = []
        , pendingRequests = []
        }

      -- Attempt a request without credentials
      postJson RequestsR $ doctorPatientRequest 1 1
      statusIs 403

      -- Attempt a request from doctor to patient
      postJsonAuth jamesCreds RequestsR $ doctorPatientRequest 1 1
      statusIs 403

      -- Add a request from the patient for the doctor
      postJsonAuth bobbyCreds RequestsR $ doctorPatientRequest 1 1
      jsonResponseIs $ Entity (requestKey 1) $ doctorPatientRequest 1 1

      -- Ensure that the doctor has the request
      getAuth jamesCreds $ DoctorR 1
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

      -- -- Ensure that the patient has the request
      getAuth bobbyCreds $ PatientR 1
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

      -- Attempt the confirm the relation as the patient
      postJsonAuth bobbyCreds RelationsR $ doctorPatientRelation 1 1
      statusIs 403

      -- Confirm the patient's request
      postJsonAuth jamesCreds RelationsR $ doctorPatientRelation 1 1
      jsonResponseIs $ Entity (relationKey 1) $ doctorPatientRelation 1 1

      -- Ensure that the doctor has the patient
      getAuth jamesCreds $ DoctorR 1
      jsonResponseIs $ DoctorWithPatients
        { id = doctorKey 1
        , firstName = "James"
        , lastName = "Hill"
        , patients =
          [ DoctorViewOfPatient
              { id = patientKey 1
              , relationId = relationKey 1
              , firstName = "Bobby"
              , lastName = "Lee"
              , dateOfBirth = Nothing
              , prescriptions = []
              }
          ]
        , pendingRequests = []
        }

      -- Ensure that the patient has the doctor
      getAuth bobbyCreds $ PatientR 1
      jsonResponseIs $ PatientWithDoctors
        { id = patientKey 1
        , firstName = "Bobby"
        , lastName = "Lee"
        , dateOfBirth = Nothing
        , doctors =
          [ PatientViewOfDoctor
            { id = doctorKey 1
            , relationId = relationKey 1
            , firstName = "James"
            , lastName = "Hill"
            }
          ]
        , pendingRequests = []
        , prescriptions = []
        }
