import checkRequestErrors from "./errors";
import settings from "../config/settings";

function getDoctorData(doctorID = '') {
  addition = doctorID == '' ? '' : '/';  
  return fetch(settings.REMOTE_SERVER_URL + settings.DOCTOR_RES + addition + doctorID)
  .then(checkRequestErrors)
  .then(response => response.json());
}

export default getDoctorData;