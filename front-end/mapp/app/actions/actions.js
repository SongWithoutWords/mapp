import getUser from "../lib/getUser";
import getDoctors from "../lib/getDoctors";

import {
  FETCHING_USER,
  FETCHING_DOCTORS,
  CLEAR_USER,
} from "../config/constants";

/* action creators */
// using promise middleware to dispatch on different actions
// based on the state of the promise object in payload
export function fetchUser(url, form) {
  return {
    type: FETCHING_USER,
    payload: getUser(url, form)
  };
}

export function fetchDoctors() {
  return {
    type: FETCHING_DOCTORS,
    payload: getDoctors()
  };
}

export function clearUser() {
  return {
    type: CLEAR_USER
  };
}
