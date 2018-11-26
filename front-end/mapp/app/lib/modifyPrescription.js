import patchData from "../lib/patchData";
import settings from "../config/settings";
import validatePrescription from "../lib/validatePrescription";
import genAlert from "../components/generalComponents/genAlert";

function modifyPrescription({
  medication = "",
  dosage = 0,
  dosageUnit = "",
  frequency = "",
  minutesBetweenDoses = 0,
  amountInitial = 0,
  startDateTime = null,
  patientID = null,
  prescriptionID = null,
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
      const url =
        settings.REMOTE_SERVER_URL +
        settings.PRESCRIPTION_RES +
        "/" +
        prescriptionID;
      const dosageSchedule = [];

      var schedule = {};
      schedule.firstDose = startDateTime;
      schedule.dosage = dosage;
      schedule.minutesBetweenDoses = minutesBetweenDoses;
      dosageSchedule.push(schedule);

      const json = {
        patient: patientID,
        medication: medication,
        dosageUnit: dosageUnit,
        amountInitial: amountInitial,
        dosageSchedule: dosageSchedule
      };

      console.log(JSON.stringify(json));

      return patchData(url, json)
        .then(response => {
          genAlert("Prescription updated!");
          navigation.goBack();
        })
        .catch(error => {
          genAlert("Failed to modify a prescription", error.message);
        });
  }
}

export default modifyPrescription;
