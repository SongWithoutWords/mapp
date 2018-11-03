import getUser from '../lib/getUser'
import getDoctors from '../lib/getDoctors'


/* action constants */
export const FETCHING_USER  = 'FETCHING_USER';
export const FETCHING_DOCTORS  = 'FETCHING_DOCTORS';
export const CLEAR_USER  = 'CLEAR_USER';

// actions constants used by promise middleware
export const FETCHING_USER_PENDING  = 'FETCHING_USER_PENDING';
export const FETCHING_USER_FULFILLED  = 'FETCHING_USER_FULFILLED';
export const FETCHING_USER_REJECTED  = 'FETCHING_USER_REJECTED';

export const FETCHING_DOCTORS_PENDING  = 'FETCHING_DOCTORS_PENDING';
export const FETCHING_DOCTORS_FULFILLED  = 'FETCHING_DOCTORS_FULFILLED';
export const FETCHING_DOCTORS_REJECTED  = 'FETCHING_DOCTORS_REJECTED';

/* action creators */
// using promise middleware to dispatch on different actions 
// based on the state of the promise object in payload
export function fetchUser(url, form){
    return{
        type: FETCHING_USER,
        payload: getUser(url, form)
    }
}

export function fetchDoctors(){
    return{
        type: FETCHING_DOCTORS,
        payload: getDoctors()
    }
}

export function clearUser(){
    return{
        type: CLEAR_USER,
    }
}