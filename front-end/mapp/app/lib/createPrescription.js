import postData from "../lib/postData";
import settings from "../config/settings";
import validatePrescription from "../lib/validatePrescription";
import genAlert from "../components/generalComponents/genAlert";
import deletePrescription from "../lib/deletePrescription";

function createPrescription({
  medication = "",
  dosage = 0,
  dosageUnit = "",
  frequency = "",
  minutesBetweenDoses = 0,
  amountInitial = 0,
  startDateTime = null,
  patientID = null,
  doctorID = null, // TODO
  navigation = null,
  email = "",
  password = ""
}) {
  if (
    validatePrescription({
      dosage,
      dosageUnit,
      frequency,
      amountInitial,
      startDateTime,
      medication
    })
  ) {
    const url = settings.REMOTE_SERVER_URL + settings.PRESCRIPTION_RES;
    const dosageSchedule = [];

    var schedule = {};
    schedule.firstDose = startDateTime;
    schedule.dosage = dosage;
    schedule.minutesBetweenDoses = minutesBetweenDoses;
    dosageSchedule.push(schedule);

    const data = {
      patient: patientID,
      doctor: doctorID,
      medication: medication,
      dosageUnit: dosageUnit,
      amountInitial: amountInitial,
      dosageSchedule: dosageSchedule
    };

    console.log(JSON.stringify(data));
    return postData(url, data, email, password)
      .then(response => {
        genAlert("A new prescription created!");
        if(navigation !== null)
          navigation.goBack();
      })
      .catch(error => {
        genAlert("Failed to create a new prescription", error.message);
      });
  }
}

export function renewPrescription({
  medication = "",
  dosage = 0,
  dosageUnit = "",
  frequency = "",
  minutesBetweenDoses = 0,
  amountInitial = 0,
  startDateTime = null,
  patientID = null,
  doctorID = null, // TODO
  navigation = null,
  email = "",
  password = "",
  prescriptionID = 0
}) {
  if (
    validatePrescription({
      dosage,
      dosageUnit,
      frequency,
      amountInitial,
      startDateTime,
      medication
    })
  ) {
    const url = settings.REMOTE_SERVER_URL + settings.PRESCRIPTION_RES;
    const dosageSchedule = [];

    var schedule = {};
    schedule.firstDose = startDateTime;
    schedule.dosage = dosage;
    schedule.minutesBetweenDoses = minutesBetweenDoses;
    dosageSchedule.push(schedule);

    console.log("from renew" + JSON.stringify(dosageSchedule));

    const data = {
      patient: patientID,
      doctor: doctorID,
      medication: medication,
      dosageUnit: dosageUnit,
      amountInitial: amountInitial,
      dosageSchedule: dosageSchedule
    };

    console.log(JSON.stringify(data));
    console.log('osgol');
    console.log('saasa' + email + ' ' + password);
    return postData(url, data, email, password)
      .then(response => {
        genAlert("A new prescription created!");
        deletePrescription({ prescriptionID: prescriptionID , navigation: null , email: email, password: password});
      })
      .catch(error => {
        genAlert("Failed to create a new prescription", error.message);
      });
  }
}

export default createPrescription;
