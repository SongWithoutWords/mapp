import settings from "../config/settings";

function getDoctors() {
  return fetch(settings.REMOTE_SERVER_URL + settings.DOCTOR_RES)
}

export default getDoctors;