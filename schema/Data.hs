module Data where

data Doctor = Doctor
  { doctorId :: Int
  , name :: String
  , patients :: [Patient]
  }

data Patient = Patient
  { patientId :: Int
  , name :: String
  , age :: Int
  , doctors :: [Doctor]
  , prescriptions :: [Prescription]
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

