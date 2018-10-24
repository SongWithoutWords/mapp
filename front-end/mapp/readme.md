### RESTful APIs (in httpie syntax)
#### create a patient account
```
http POST http://www.agis-mapp.xyz/patients 
email=asdsadfdsfadsf@black.com
password=jblack 
firstName=Test 
lastName=Test
```
------
#### create a doctor account (dataOfBrith field rn is just null)
```
http POST http://www.agis-mapp.xyz/doctors
email=asdsadfdsfadsf@black.com 
password=jblack 
firstName=Test 
lastName=Test
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




## TODO for the MVP 
- sign-up/sign-in screen (text inputs + picker for doctor or patient account) 
    - make a post request to the server
    - return a message: sign in fail/ sign up fail/sign up sucess/sign in sucess
    - give different layouts of the app based on the signed-in user type
- doctor detail info screen
    - probably have info like location, telephone, email, avatar, and specialty.
    - a button for the patient to make a request (and information about himself/herself)
        - onPress => make a POST to server
- notificaiton screen info screen
    - a material top tab navigation inside the existing bottom tab nav 
        - message tab (used later for patient-doctor communication) + notification tab 
    - patient: notification about his/her requests (to doctor/to renew prescription..)
    - doctor: notification about who is requesting to be his/her patient
        - onPress => a button for accept/decline (and give info about the patient)
        - make a POST to the server based on which button is pressed

## TODO for this week
- redux + redux persist for mutate states
    - actions/reducers/store
    - do research on medicine (json returned by drug apis) and create data structure for prescription
- prescription edit (edit button on the top left of the info screen) + add new prescription screen (modal + fab ?)

## Get started
- git clone
- npm install 
- react-native run-ios OR react-native run-android
    - make sure you are in mapp/front-end/mapp/app
    - make sure you are in front-end branch
    - make sure Android Virtual Device is running
### Directory Structure
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

### Troubleshooting
```
java.lang.string cannot be cast to com.facebook.react.uimanager.accessibility DelegateUtil$accessibilityRole
```
 - [I have Some Error when add Button to my React Native App?](https://stackoverflow.com/questions/52784633/i-have-some-error-when-add-button-to-my-react-native-app?noredirect=1#comment92491686_52784633)

```
error: bundling failed: Error: Unable to resolve module `./../react-transform-hmr/lib/index.js
```
- [error: bundling failed: Error: Unable to resolve module ](https://github.com/facebook/react-native/issues/21490)

## TODO list
### UI  
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

### Core Functinalities 
- authentication
- communication/sync with server (how? when? how often?)
- on-device persistent storage of user data
- scheduling timers/notifications for prescriptions
- creating patient-doctor relationship
- retrieving medication data (using APIs or building our own DB?)


### Stretch goals
- How do we integrate fuzzy find algorithm (on-device? server-side?)
- Google sign-in integration
- Integrate chat feature + sharing medical document?


### Libraries 
- UI components:
    - React Native Elements 
    - Native Base
    - react-native-material-ui
    - react navigation
