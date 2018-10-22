## TODO list
#### Get started
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

#### Troubleshooting
```
java.lang.string cannot be cast to com.facebook.react.uimanager.accessibility DelegateUtil$accessibilityRole
```
 - [I have Some Error when add Button to my React Native App?](https://stackoverflow.com/questions/52784633/i-have-some-error-when-add-button-to-my-react-native-app?noredirect=1#comment92491686_52784633)

```
error: bundling failed: Error: Unable to resolve module `./../react-transform-hmr/lib/index.js
```
- [error: bundling failed: Error: Unable to resolve module ](https://github.com/facebook/react-native/issues/21490)
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
- scheduling timers/notifications for prescriptions
- creating patient-doctor relationship
- communication/sync with server (how? when? how often?)
- on-device persistent storage of user data
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
- Navigation:
    - react navigation
