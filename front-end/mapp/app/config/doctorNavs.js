import React from "react";
import { AppRegistry } from "react-native";
import settings from "../config/settings";
import { createMaterialBottomTabNavigator } from "react-navigation-material-bottom-tabs";
import MaterialCommunityIcons from "react-native-vector-icons/MaterialCommunityIcons";
import MaterialIcons from "react-native-vector-icons/MaterialIcons";
import { createStackNavigator } from "react-navigation";

import DoctorInboxScreen from "../screens/DoctorInboxScreen";
import AccountScreen from "../screens/AccountScreen";
import PatientListScreen from "../screens/PatientListScreen";
import PatientInfoScreen from "../screens/PatientInfoScreen";
import EditPrescriptionView from "../screens/EditPrescriptionView";
import { genTabNavOptions } from "../lib/genNavOptions";

import MakePrescriptionView from "../screens/MakePrescriptionView";

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
  },
  DoctorEditPrescription: {
    screen: EditPrescriptionView,
    navigationOptions: {
      title: "Edit Prescription",
      headerForceInset: { top: "never", bottom: "never" }
    }
  },
  DoctorMakePrescription: {
    screen: MakePrescriptionView,
    navigationOptions: {
      title: "New Prescription",
      headerForceInset: { top: "never", bottom: "never" }
    }
  }
});

const DoctorTabNavOptions = genTabNavOptions("Account", [
  "Inbox",
  "PatientList",
  "Account"
]);
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
      screen: DoctorInboxScreen,
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
AppRegistry.registerComponent("DoctorTabNavigator", () => DoctorTabNavigator);
