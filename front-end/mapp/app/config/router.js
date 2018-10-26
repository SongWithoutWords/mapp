import { createStackNavigator, createSwitchNavigator } from "react-navigation";

import SignUpScreen from "../screens/SignUpScreen";
import SignInScreen from "../screens/SignInScreen";
import WelcomeScreen from "../screens/WelcomeScreen";
import PatienTabNavigator from "./patientNavs";
import DoctorTabNavigator from "./doctorNavs";
import AccountScreen from "../screens/AccountScreen";

// a stack navigator for sign up and sign in
export const AuthStackNavigator = createStackNavigator(
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
    }
  },
  {
    initialRouteName: "Welcome"
  }
);

export const AuthSwitchNavigator = createSwitchNavigator(
  {
    AuthStack: {
      screen: AuthStackNavigator
    },
    Account: {
      screen: AccountScreen
    },
    PatientTab: {
      screen: PatienTabNavigator,
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
    initialRouteName: "AuthStack"
  }
);
