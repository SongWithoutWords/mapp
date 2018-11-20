import _ from "lodash";
import { USER_TYPE } from "../config/constants";

// signin/signup api calls
function getUser(url, form) {
  const { email, password, firstName = "", lastName = "" } = form;
  const jsonBody = { email, password, firstName, lastName };
  return fetch(url, {
    method: "POST",
    headers: {
      Accept: "application/json",
      "Content-Type": "application/json"
    },
    body: JSON.stringify(jsonBody)
  })
    .then(response => {
      if (!response.ok) {
        throw new Error(response.status + ' Password or Email is incorrect');
      }
      return response.json();
    })
    .then(function(responseJson) {
      result = {};

      // user
      const userTypeString =
        "doctors" in responseJson ? USER_TYPE.PATIENT : USER_TYPE.DOCTOR;
      const user = _.merge({}, form, responseJson); // response json may overwrite attributes in form
      let { doctors = [], patients = [], pendingRequests = [] } = user;
      const myDoctors = doctors.map(x => x.id);
      const myPatients = patients.map(x => x.id);

      // change requestid to id
      const pendingRequests = pendingRequests.map(x => {
        x.id = x.requestId;
        delete x['requestId'];
        return x;
      }); 
      console.log(pendingRequests);

      const myPendingRequests = pendingRequests.map(x => x.id); 
      delete user.doctors;
      delete user.patients;
      delete user.pendingRequests;
      user.myDoctors = myDoctors;
      user.myPatients = myPatients;
      user.myPendingRequests = myPendingRequests;
      user.userType = userTypeString;
      user.myPrescriptions = [{ id : 1 // TODO
      , doctor : 1
      , patient : 1
      , medication : "Cefixime 400"
      , dosageUnit : "Pills"
      , amountInitial : 100
      , amountRemaining : 60
      }];
      console.log(user);

      // patients and doctors
      patients = arrayToObj(patients);
      doctors = arrayToObj(doctors);

      // pendingRequests
      pendingRequests = arrayToObj(pendingRequests); 
      // TODO: duplication of patient obj and doctor obj (should be single source of truth)
      // need to change inbox screen

      let prescriptions = arrayToObj(user.myPrescriptions);
      // prescriptions
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
