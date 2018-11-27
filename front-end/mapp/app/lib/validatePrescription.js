import genAlert from "../components/generalComponents/genAlert";
export default (validatePrescription = ({
  dosage,
  dosageUnit,
  frequency,
  amountInitial,
  startDateTime,
  medication
}) => {
  var msg = null;
  if (medication === "") {
    msg = "Invalid medication name";
  } else if (amountInitial <= 0) {
    msg = "Invalid initial amount";
  } else if (dosage <= 0 || dosage > amountInitial) {
    msg = "Invalid dosage";
  } else if (dosageUnit === "") {
    msg = "Invalid dosage unit";
  } else if (frequency === "") {
    msg = "Invalid frequency";
  } else if (startDateTime === null || startDateTime < new Date()) {
    msg = "Invalid start date";
  } else if (!divisible(round2decimal(amountInitial), round2decimal(dosage))) {
    msg = "Initial amount is not divisible by dosage";
  }

  if (msg !== null) {
    genAlert("Invalid prescription", msg);
    return false;
  } else {
    return true;
  }
});

export function round2decimal(a) {
  return Math.round(a * 100) / 100;
}

function divisible(a, b) {
  return ((a * 100) % (b * 100)) / 100 === 0;
}
