export const CLEAR_USER = "CLEAR_USER";
export const FETCHING_USER = "FETCHING_USER";
export const FETCHING_DOCTORS = "FETCHING_DOCTORS";

// actions constants used by promise middleware
export const FETCHING_USER_PENDING = "FETCHING_USER_PENDING";
export const FETCHING_USER_FULFILLED = "FETCHING_USER_FULFILLED";
export const FETCHING_USER_REJECTED = "FETCHING_USER_REJECTED";

export const FETCHING_DOCTORS_PENDING = "FETCHING_DOCTORS_PENDING";
export const FETCHING_DOCTORS_FULFILLED = "FETCHING_DOCTORS_FULFILLED";
export const FETCHING_DOCTORS_REJECTED = "FETCHING_DOCTORS_REJECTED";

// user type enum
let PATIENT = "patient";
let DOCTOR = "doctor";
export const USER_TYPE = {
  PATIENT,
  DOCTOR
};

// frequency enum
let EVERY_WEEK = "every week";
let EVERY_DAY = "every day";
let EVERY_MINUTE = "every minute";

export const FREQUENCY = {
  EVERY_DAY,
  EVERY_WEEK,
  EVERY_MINUTE
};

// dosage unit enum
let GRAM = "Gram";
let LITER = "Liter";
export const DOSAGE_UNIT = {
  GRAM,
  LITER
};