import settings from "../config/settings";
import genAlert from "../components/generalComponents/genAlert";
import fetchAuth from "../lib/fetchAuth";

function deletePrescription({
  prescriptionID = null,
  navigation = null,
  email = "",
  password = ""
}) {
  const url =
    settings.REMOTE_SERVER_URL +
    settings.PRESCRIPTION_RES +
    "/" +
    prescriptionID;
  const method = "DELETE";
  return fetchAuth({url, method, email, password})
    .then(response => {
      genAlert("Prescription deleted!");
      if(navigation !== null)
        navigation.goBack();
    })
    .catch(error => {
      genAlert("Failed to delete a prescription", error.message);
    });
}

export default deletePrescription;
