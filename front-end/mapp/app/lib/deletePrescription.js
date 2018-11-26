import settings from "../config/settings";
import genAlert from "../components/generalComponents/genAlert";
import checkRequestErrors from "../lib/errors";


function deletePrescription({ prescriptionID = null, navigation = null }) {
  const url =
    settings.REMOTE_SERVER_URL +
    settings.PRESCRIPTION_RES +
    "/" +
    prescriptionID;
  console.log("whyyyyyy");
  console.log(url);

  return fetch(url, {
    // TODO add email and password to header
    method: "DELETE",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json"
    }
  }).then(checkRequestErrors)
    .then(response => {
      genAlert("Prescription deleted!");
      navigation.goBack();
    })
    .catch(error => {
      genAlert("Failed to delete a prescription", error.message);
    });
}

export default deletePrescription;
