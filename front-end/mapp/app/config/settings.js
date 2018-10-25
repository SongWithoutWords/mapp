let LOCAL_SERVER_URL = "http://localhost:3000";
let REMOTE_SERVER_URL = "http://www.agis-mapp.xyz";

//RESTful APIs
let DOCTOR_RES = "/doctors";
// create a doctor user
// request info about a doctor

let PATIENT_RES = "/patients";
// create a patient user

let REQUESTS_RES = "/requests";
// create a relationship
// add a request from patient to doctor
// confirm the patient's request

let RELAITON_RES = "/relations";
// accept a pending request

export const settings = {
  LOCAL_SERVER_URL,
  REMOTE_SERVER_URL,
  DOCTOR_RES,
  PATIENT_RES,
  REQUESTS_RES,
  RELAITON_RES
};

export default settings;
