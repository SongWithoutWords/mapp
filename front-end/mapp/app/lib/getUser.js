import _ from "lodash";
import { USER_TYPE } from "../config/constants";
import fetchAuth from "./fetchAuth";

// signin/signup api calls
function getUser(url, form) {
  const { email, password, firstName = "", lastName = "" } = form;
  const method = "POST";
  const data = { email, password, firstName, lastName };

  return fetchAuth({ url, method, data, email, password })
    .then(response => {
      return response.json();
    })
    .then(function(responseJson) {
      result = {};

      // user
      const userTypeString =
        "doctors" in responseJson ? USER_TYPE.PATIENT : USER_TYPE.DOCTOR;
      const user = _.merge({}, form, responseJson); // response json may overwrite attributes in form
      let {
        doctors = [],
        patients = [],
        pendingRequests = [],
        prescriptions = []
      } = user;
      const myDoctors = doctors.map(x => x.id);
      const myPatients = patients.map(x => x.id);
      const myPrescriptions = prescriptions.map(x => x.id);

      // change requestid to id
      pendingRequests = pendingRequests.map(x => {
        x.id = x.requestId;
        delete x["requestId"];
        return x;
      });
      console.log(pendingRequests);

      const myPendingRequests = pendingRequests.map(x => x.id);

      delete user.doctors;
      delete user.patients;
      delete user.pendingRequests;
      delete user.prescriptions;

      user.myDoctors = myDoctors;
      user.myPatients = myPatients;
      user.myPendingRequests = myPendingRequests;
      user.myPrescriptions = myPrescriptions;
      user.userType = userTypeString;
      console.log(user);

      patients = arrayToObj(patients);
      console.log(patients);
      doctors = arrayToObj(doctors);
      prescriptions = arrayToObj(prescriptions);
      pendingRequests = arrayToObj(pendingRequests);
      // TODO: duplication of patient obj and doctor obj (should be single source of truth)
      // need to change inbox screen

      result = {
        user,
        patients,
        doctors,
        pendingRequests,
        prescriptions
      };
      console.log(result);
      return result;
    });
}

function arrayToObj(array) {
  const result = {};
  const byId = {};
  const allIds = [];

  array.forEach(element => {
    byId[element.id] = element;
    allIds.push(element.id);
  });

  result.byId = byId;
  result.allIds = allIds;
  return result;
}
export default getUser;
