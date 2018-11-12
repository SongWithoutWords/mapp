import React from "react";
import { AppRegistry } from 'react-native';
import settings from "../config/settings";
import { createMaterialBottomTabNavigator } from "react-navigation-material-bottom-tabs";
import MaterialCommunityIcons from "react-native-vector-icons/MaterialCommunityIcons";
import MaterialIcons from "react-native-vector-icons/MaterialIcons";
import { createStackNavigator } from "react-navigation";

import InboxScreen from "../screens/InboxScreen";
import AccountScreen from "../screens/AccountScreen";
import MakePrescriptionView from "../screens/MakePrescriptionView";
import PatientListScreen from "../screens/PatientListScreen";
import PatientInfoScreen from "../screens/PatientInfoScreen";
import {genTabNavOptions} from "../lib/genNavOptions"

const PatientStackNavigator = createStackNavigator({
  PatientList: {
    screen: PatientListScreen,
    navigationOptions: {
      header: null,
      headerForceInset: { top: "never", bottom: "never" }
    }
  },
  PatientInfo: {
    screen: PatientInfoScreen,
    navigationOptions: {
      title: "Patient Info",
      headerForceInset: { top: "never", bottom: "never" }
    }
  }
});

const DoctorTabNavOptions = genTabNavOptions("PatientList", ["Inbox", "PatientList", "Account"]);
// tab navigator for doctor
const DoctorTabNavigator = createMaterialBottomTabNavigator(
  {
    // screens and their navigation options
    PatientList: {
      screen: PatientStackNavigator,
      navigationOptions: {
        tabBarLabel: "Patients",
        tabBarIcon: ({ tintColor }) => (
          <MaterialIcons name="people" size={25} color={tintColor} />
        )
      }
    },
    Inbox: {
      screen: MakePrescriptionView,
      navigationOptions: {
        tabBarLabel: "Inbox",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="inbox" size={25} color={tintColor} />
        )
      }
    },
    Account: {
      screen: AccountScreen,
      navigationOptions: {
        tabBarLabel: "Account",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons
            name="account-circle"
            size={25}
            color={tintColor}
          />
        )
      }
    }
  },
  DoctorTabNavOptions
);
export default DoctorTabNavigator;
AppRegistry.registerComponent('DoctorTabNavigator', () => DoctorTabNavigator);
