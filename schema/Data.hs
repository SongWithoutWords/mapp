module Data where

data Doctor = Doctor
  { doctorId :: Int
  , name :: String
  }

data DoctorWithPatients = DoctorWithPatients
  { doctor :: Doctor
  , patients :: [Patient]
  }

data Patient = Patient
  { patientId :: Int
  , name :: String
  , age :: Int
  , prescriptions :: [Prescription]
  }

data PatientWithDoctors = PatientWithDoctors
  { patient :: Patient
  , doctors :: [Doctor]
  }

data Prescription = Prescription
  { prescriptionId :: Int
  , prescribingDoctor :: Doctor
  , medication :: Medication
  , dosageUnit :: DosageUnit
  , amountInitial :: Double
  , amountRemaining :: Double
  , dosageSchedule :: [RecurringDose]
  }

data Medication = Medication
  { medicationId :: Int
  , name :: String
  }

data RecurringDose = RecurringDose
  { firstDose :: DateTime
  , minutesBetweenDoses :: Int
  , dosageAmount :: Double
  }

data DosageUnit
  = Gram
  | Liter

