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
import { genTabNavOptions } from "../lib/genNavOptions";

import MakePrescriptionView from "../screens/MakePrescriptionView";

// GUI testing
import { hook } from 'cavy'
import IconWithRef from "../lib/IconWithRef";
const TestableIcon = hook(IconWithRef);

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
      navigationOptions: ({ navigation }) => ({
        tabBarLabel: "Patients",
        tabBarIcon: ({ tintColor }) =>{
          const { routeName } = navigation.state;
          return (
            <TestableIcon
              name="people"
              size={25}
              color={tintColor}
              navigation={navigation}
              routeName={routeName}
              iconType={MaterialIcons}
            />
          );
        } 
      })
    },
    Inbox: {
      screen: DoctorInboxScreen,
      navigationOptions: ({ navigation }) => ({
        tabBarLabel: "Inbox",
        tabBarIcon: ({ tintColor }) => {
          const { routeName } = navigation.state;
          return (
            <TestableIcon
              name="inbox"
              size={25}
              color={tintColor}
              navigation={navigation}
              routeName={routeName}
              iconType={MaterialCommunityIcons}
            />
          );
        }
      })
    },
    Account: {
      screen: AccountScreen,
      navigationOptions: ({ navigation }) => ({
        tabBarLabel: "Account",
        tabBarIcon: ({ tintColor }) => {
          const { routeName } = navigation.state;
          return (
            <TestableIcon
              name="account-circle"
              size={25}
              color={tintColor}
              navigation={navigation}
              routeName={routeName}
              iconType={MaterialCommunityIcons}
            />
          );
        }
      })
    }
  },
  DoctorTabNavOptions
);
export default DoctorTabNavigator;
AppRegistry.registerComponent("DoctorTabNavigator", () => DoctorTabNavigator);
