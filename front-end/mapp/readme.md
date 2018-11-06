<!-- TOC -->

- [RESTful APIs (in httpie syntax)](#restful-apis-in-httpie-syntax)
    - [available Routes](#available-routes)
    - [create a patient account](#create-a-patient-account)
    - [create a doctor account (dataOfBrith field rn is just null)](#create-a-doctor-account-dataofbrith-field-rn-is-just-null)
    - [get all doctor info](#get-all-doctor-info)
    - [get a doctor's info (the doctor's id is 1 in this case)](#get-a-doctors-info-the-doctors-id-is-1-in-this-case)
    - [add a request from patient 1 to doctor 1's pending request list](#add-a-request-from-patient-1-to-doctor-1s-pending-request-list)
    - [doctor1 accepts the request from patient1](#doctor1-accepts-the-request-from-patient1)
- [Get started](#get-started)
    - [Directory Structure](#directory-structure)
    - [Redux, Thunk and Promise Middleware](#redux-thunk-and-promise-middleware)
    - [Troubleshooting](#troubleshooting)
- [TODO list](#todo-list)
    - [UI](#ui)
    - [Core Functinalities](#core-functinalities)
    - [Stretch goals](#stretch-goals)
    - [Libraries](#libraries)

<!-- /TOC -->
### RESTful APIs (in httpie syntax)
#### available Routes
- https://github.com/SongWithoutWords/mapp/blob/ed7c56f141c18c12c6697be67588016dfe92c0bc/server/config/routes

- search keywords (e.g. PatientR, DoctorR) in github to figure out request examples
- NOTE: examples below may be outdated
#### create a patient account
```
http POST http://www.agis-mapp.xyz/patients 
email=asdsadfdsfadsf@black.com
password=jblack 
firstName=Test 
lastName=Test

return:
{
    "dateOfBirth": null,
    "doctors": [],
    "firstName": "fan",
    "id": 1,
    "lastName": "no",
    "pendingRequests": []
}
```
------
#### create a doctor account (dataOfBrith field rn is just null)
```
http POST http://www.agis-mapp.xyz/doctors
email=asdsadfdsfadsf@black.com 
password=jblack 
firstName=Test 
lastName=Test

return:
{
    "firstName": "fan",
    "id": 1,
    "lastName": "no",
    "patients": [],
    "pendingRequests": []
}
```

------
#### get all doctor info
```
http GET http://www.agis-mapp.xyz/doctors
```

------
#### get a doctor's info (the doctor's id is 1 in this case)
```
http GET http://www.agis-mapp.xyz/doctors/1
```

------
#### add a request from patient 1 to doctor 1's pending request list
return null if the request already exists
":=" is just the httpie syntax to indicate an integer value
```
http POST http://www.agis-mapp.xyz/requests
doctor:=1 
patient:=1
```

------

#### doctor1 accepts the request from patient1
```
http POST  http://www.agis-mapp.xyz/relations 
patient:=1 
doctor:=1
```
return this if such pending request does not exist:
```
{
    "errors": [
        "No pending request from this patient to this doctor"
    ],
    "message": "Invalid Arguments"

```



### Get started
- git clone
- npm install 
- react-native run-ios OR react-native run-android
    - make sure you are in mapp/front-end/mapp/app
    - make sure you are in front-end branch
    - make sure Android Virtual Device is running
#### Directory Structure
- /app      
    - the main folder we will be working in 
- /app/componets
    - general components
- /app/config
    - router.js 
        - handling navigation and navigation options
- /app/screens
    - all the screens (views)
- /App.js
    - root component
#### Redux, Thunk and Promise Middleware
- Please read these:
    - https://www.one-tab.com/page/aQ_PTKu7SqqI0koEjYFaCg
- To debug Redux, install these two (install the second one as a npm package):
    - https://github.com/jhen0409/react-native-debugger
    - https://github.com/zalmoxisus/redux-devtools-extension

- related files are in /store, /reducers, /actions


#### Troubleshooting
[unable to resolve module](https://github.com/facebook/react-native/issues/4968)
```
setState() does not always immediately update the component. It may batch or defer the update until later. This makes reading this.state right after calling setState() a potential pitfall. Instead, use componentDidUpdate or a setState callback (setState(updater, callback)), either of which are guaranteed to fire after the update has been applied. If you need to set the state based on the previous state, read about the updater argument below.
```
```
java.lang.string cannot be cast to com.facebook.react.uimanager.accessibility DelegateUtil$accessibilityRole
```
 - [I have Some Error when add Button to my React Native App?](https://stackoverflow.com/questions/52784633/i-have-some-error-when-add-button-to-my-react-native-app?noredirect=1#comment92491686_52784633)

```
error: bundling failed: Error: Unable to resolve module `./../react-transform-hmr/lib/index.js
```
- [error: bundling failed: Error: Unable to resolve module ](https://github.com/facebook/react-native/issues/21490)



### TODO list
https://github.com/SongWithoutWords/mapp/issues
#### UI  
For patients:
- prescription list screen (like a list of cards)
    - each card could be a component
- prescription detail screen (tap on a card => expanding to this screen)
    - buttons to edit the prescription
- doctor list screen 
- doctor detail screen
    - buttons to send a request

For doctors:
- patient list screen
- patient detail screen

For both:
- message and notification screen (similar to youtube probably)
- Login/Signup screens
- Search bar component
- profile/settings screen

#### Core Functinalities 
- authentication
- communication/sync with server (how? when? how often?)
- on-device persistent storage of user data
- scheduling timers/notifications for prescriptions
- creating patient-doctor relationship
- retrieving medication data (using APIs or building our own DB?)


#### Stretch goals
- How do we integrate fuzzy find algorithm (on-device? server-side?)
- Google sign-in integration
- Integrate chat feature + sharing medical document?


#### Libraries 
- UI components:
    - React Native Elements 
    - Native Base
    - react-native-material-ui
    - react navigation
