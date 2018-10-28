import React from "react";
import { AppRegistry } from 'react-native';
import settings from "../config/settings";
import { createMaterialBottomTabNavigator } from "react-navigation-material-bottom-tabs";
import MaterialCommunityIcons from "react-native-vector-icons/MaterialCommunityIcons";
import MaterialIcons from "react-native-vector-icons/MaterialIcons";
import { createStackNavigator } from "react-navigation";

import InboxScreen from "../screens/InboxScreen";
import AccountScreen from "../screens/AccountScreen";
import PatientListScreen from "../screens/PatientListScreen";
import PatientInfoScreen from "../screens/PatientInfoScreen";

const DoctorStackNavigator = createStackNavigator({
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
      title: "Doctor Info",
      headerForceInset: { top: "never", bottom: "never" }
    }
  }
});

// tab navigator for doctor
const DoctorTabNavigator = createMaterialBottomTabNavigator(
  {
    // screens and their navigation options
    PatientList: {
      screen: DoctorStackNavigator,
      navigationOptions: {
        tabBarLabel: "Patients",
        tabBarIcon: ({ tintColor }) => (
          <MaterialIcons name="people" size={25} color={tintColor} />
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
  {
    initialRouteName: "PatientList",
    shifting: true,
    activeColor: settings.ACTIVE_COLOR,
    inactiveColor: settings.INACTIVE_COLOR,
    barStyle: { backgroundColor: settings.THEME_COLOR },
    tabBarOptions: {
      showIcon: true,
      labelStyle: { fontSize: 10 }
    },
    order: ["Inbox", "PatientList", "Account"]
  }
);

class DoctorTabNavContainer extends React.Component {
  handleOnPress = () => {this.props.navigation.navigate("AuthStack")}
  static router = DoctorTabNavigator.router;
  render() {
    const { navigation } = this.props;
    const firstName = navigation.getParam("firstName", "");
    const lastName = navigation.getParam("lastName", "");
    const id = navigation.getParam("id", "");
    let props = {
      id: id,
      firstName: firstName,
      lastName: lastName,
      handleOnPress: this.handleOnPress
    }
    // pass user info to screens in the patient tab navigator
    return <DoctorTabNavigator navigation={this.props.navigation} screenProps={props} />;
  }
}
export default DoctorTabNavContainer;
AppRegistry.registerComponent('DoctorTabNavContainer', () => DoctorTabNavContainer);
