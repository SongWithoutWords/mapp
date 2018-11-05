import _ from "lodash";
import {
 FETCHING_USER_PENDING,
 FETCHING_USER_FULFILLED,
 FETCHING_USER_REJECTED,  
 FETCHING_DOCTORS_PENDING,
 FETCHING_DOCTORS_FULFILLED,
 FETCHING_DOCTORS_REJECTED,
 CLEAR_USER
  } from '../config/constants' 

// user normalized data structure rather than nested components
// more info see: https://redux.js.org/recipes/structuringreducers/normalizingstateshape
const initialState = {
    isFetchingUser:false,
    fetchingUserError:false,
    isFetchingDoctors:false,
    fetchingDoctorError:false,
    isLoggedIn: false,
    user:{
        id:-1,
        firstName:'',
        lastName:'',
        email:'',
        password:'',
        userType:'',
        dateOfBirth:null,
        myDoctors:[], 
        myPatients:[],
        myPendingRequests: []
    },
    patients:{ 
        byId:{},
        allIds:[]
    },
    doctors:{ 
        byId:{},
        allIds:[]
    },
    pendingRequests:{
        byId:{},
        allIds:[]
    },
    prescriptions:{
        byId:{},
        allIds:[]
    }
}
export default function dataReducer(state=initialState, action) {
    switch (action.type) {
        case CLEAR_USER:
            return initialState
        case FETCHING_USER_PENDING:
            return {
                ...state,
                isFetchingUser: true
            }
        case FETCHING_USER_FULFILLED:
            return {
                ...state,
                user: action.payload.user,
                patients: { 
                    byId: _.merge({}, state.patients.byId, action.payload.patients.byId), 
                    allIds: _.union([], state.patients.allIds, action.payload.allIds)
                },
                doctors: { 
                    byId: _.merge({}, state.doctors.byId, action.payload.doctors.byId), 
                    allIds: _.union([], state.doctors.allIds, action.payload.doctors.allIds)
                },
                pendingRequests: { 
                    byId: _.merge({}, state.pendingRequests.byId, action.payload.pendingRequests.byId), 
                    allIds: _.union([], state.pendingRequests.allIds, action.payload.pendingRequests.allIds)
                },
                isFetchingUser: false,
                isLoggedIn: true,
            }
        case FETCHING_USER_REJECTED: 
            return {
                ...state,
                user: {},
                isFetchingUser: false,
                fetchingUserError: true,
                isLoggedIn: false,
            }

        case FETCHING_DOCTORS_PENDING:
            return {
                ...state,
                isFetchingDoctors: true
            }
        case FETCHING_DOCTORS_FULFILLED:
            return {
                ...state,
                doctors: {},
                isFetchingDoctors: false,
            }
        case FETCHING_DOCTORS_REJECTED:
            return {
                ...state,
                doctors: {},
                isFetchingDoctors: false,
                fetchingDoctorError: true,
            }
        default:
          return state
      }
    }