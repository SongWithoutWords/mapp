import { createStackNavigator } from "react-navigation";

import SignUpScreen from "../screens/SignUpScreen";
import SignInScreen from "../screens/SignInScreen";
import WelcomeScreen from "../screens/WelcomeScreen";
import PatientTabNavigator from "./patientNavs";
import DoctorTabNavigator from "./doctorNavs";

// a stack navigator for sign up and sign in
export const AppStackNavigator = createStackNavigator(
  {
    SignIn: {
      screen: SignInScreen,
      navigationOptions: {
        headerForceInset: { top: "never", bottom: "never" }
      }
    },
    SignUp: {
      screen: SignUpScreen,
      navigationOptions: {
        title: "Sign Up",
        headerForceInset: { top: "never", bottom: "never" }
      }
    },
    Welcome: {
      screen: WelcomeScreen,
      navigationOptions: {
        header: null,
        headerForceInset: { top: "never", bottom: "never" }
      }
    },
    PatientTab: {
      screen: PatientTabNavigator,
      navigationOptions: {
        header: null,
        headerForceInset: { top: "never", bottom: "never" }
      }
    },
    DoctorTab: {
      screen: DoctorTabNavigator,
      navigationOptions: {
        header: null,
        headerForceInset: { top: "never", bottom: "never" }
      }
    }
  },
  {
    initialRouteName: "Welcome"
  }
);
