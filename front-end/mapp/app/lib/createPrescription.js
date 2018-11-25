import postData from "../lib/postData";
import settings from "../config/settings";
import validatePrescription from "../lib/validatePrescription";
import genAlert from "../components/generalComponents/genAlert";

async function createPrescription({
  medication = "",
  dosage = 0,
  dosageUnit = "",
  frequency = "",
  minutesBetweenDoses = 0,
  amountInitial = 0,
  startDateTime = null,
  patientID = null,
  doctorID = null, // TODO
  navigation = null
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

    const json = {
      patient: patientID,
      doctor: doctorID,
      medication: medication,
      dosageUnit: dosageUnit,
      amountInitial: amountInitial,
      dosageSchedule: dosageSchedule
    };

    console.log(JSON.stringify(json));
    return postData(url, json)
      .then(response => {
        genAlert("A new prescription created!");
        navigation.goBack();
      })
      .catch(error => {
        genAlert("Failed to create a new prescription", error.message);
      });
  }
}

export default createPrescription;
