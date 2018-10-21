import React from "react";
import { createMaterialBottomTabNavigator } from "react-navigation-material-bottom-tabs";
import MaterialCommunityIcons from "react-native-vector-icons/MaterialCommunityIcons";
import { createStackNavigator } from "react-navigation";

import DoctorListScreen from "../screens/DoctorListScreen";
import TestScreen from "../screens/TestScreen";
import InboxScreen from "../screens/InboxScreen";

import PrescriptionInfoScreen from "../screens/PrescriptionInfoScreen";
import PrescriptionListScreen from "../screens/PrescriptionListScreen";

const PrescriptionStackNavigator = createStackNavigator({
  PrescriptionList: {
    screen: PrescriptionListScreen,
    navigationOptions: {
      header: null,
      headerForceInset: { top: 'never', bottom: 'never' }
    }
  },
  PrescriptionInfo: {
    screen: PrescriptionInfoScreen,
    navigationOptions: {
      headerForceInset: { top: "never", bottom: "never" }
    }
  }
});

export const Tabs = createMaterialBottomTabNavigator(
  {
    // screens and their navigation options
    Test: {
      screen: TestScreen,
      navigationOptions: {
        tabBarLabel: "Test",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="settings" size={25} color={tintColor} />
        )
      }
    },
    PrescriptionList: {
      screen: PrescriptionStackNavigator,
      navigationOptions: {
        tabBarLabel: "Prescriptions",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="pill" size={25} color={tintColor} />
        )
      }
    },
    DoctorList: {
      screen: DoctorListScreen,
      navigationOptions: {
        tabBarLabel: "Doctors",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="doctor" size={25} color={tintColor} />
        )
      }
    },
    Inbox: {
      screen: InboxScreen,
      navigationOptions: {
        tabBarLabel: "Inbox",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="inbox" size={25} color={tintColor} />
        )
      }
    }
  },
  {
    initialRouteName: "PrescriptionList",
    shifting: true,
    activeColor: "#f0edf6",
    inactiveColor: "#3e2465",
    barStyle: { backgroundColor: "#694fad" },
    tabBarOptions: {
      showIcon: true,
      labelStyle: { fontSize: 10 }
    }
  }
);
