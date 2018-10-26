import React from "react";
import settings from "../config/settings";
import { createMaterialBottomTabNavigator } from "react-navigation-material-bottom-tabs";
import MaterialCommunityIcons from "react-native-vector-icons/MaterialCommunityIcons";
import { createStackNavigator } from "react-navigation";
import InboxScreen from "../screens/InboxScreen";
import AccountScreen from "../screens/AccountScreen";
import PrescriptionInfoScreen from "../screens/PrescriptionInfoScreen";
import PrescriptionListScreen from "../screens/PrescriptionListScreen";
import DoctorListScreen from "../screens/DoctorListScreen";
import DoctorInfoScreen from "../screens/DoctorInfoScreen";

// stack navigators
const PrescriptionStackNavigator = createStackNavigator({
  PrescriptionList: {
    screen: PrescriptionListScreen,
    navigationOptions: {
      header: null,
      headerForceInset: { top: "never", bottom: "never" }
    }
  },
  PrescriptionInfo: {
    screen: PrescriptionInfoScreen,
    navigationOptions: {
      title: "Prescription Info",
      headerForceInset: { top: "never", bottom: "never" }
    }
  }
});

const DoctorStackNavigator = createStackNavigator({
  DoctorList: {
    screen: DoctorListScreen,
    navigationOptions: {
      header: null,
      headerForceInset: { top: "never", bottom: "never" }
    }
  },
  DoctorInfo: {
    screen: DoctorInfoScreen,
    navigationOptions: {
      title: "Doctor Info",
      headerForceInset: { top: "never", bottom: "never" }
    }
  }
});

// tab navigator for patient
const PatientTabNavigator = createMaterialBottomTabNavigator(
  {
    // screens and their navigation options
    Inbox: {
      screen: InboxScreen,
      navigationOptions: {
        tabBarLabel: "Inbox",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="inbox" size={25} color={tintColor} />
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
      screen: DoctorStackNavigator,
      navigationOptions: {
        tabBarLabel: "Doctors",
        tabBarIcon: ({ tintColor }) => (
          <MaterialCommunityIcons name="doctor" size={25} color={tintColor} />
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
    initialRouteName: "PrescriptionList",
    shifting: true,
    activeColor: settings.ACTIVE_COLOR,
    inactiveColor: settings.INACTIVE_COLOR,
    barStyle: { backgroundColor: settings.THEME_COLOR },
    tabBarOptions: {
      showIcon: true,
      labelStyle: { fontSize: 10 }
    },
    order: ["Inbox", "PrescriptionList", "DoctorList", "Account"]
  }
);

class PatientTabNavContainer extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      lastName: "",
      firstName: "",
      id: "",
    };
  }

  componentDidMount() {
    const { navigation } = this.props;
    const firstName = navigation.getParam("firstName", "");
    const lastName = navigation.getParam("lastName", "");
    const id = navigation.getParam("id", "");
    console.log(firstName);
    console.log(lastName);
    console.log(id);
    this.setState({ lastName });
    this.setState({ firstName });
    this.setState({ id });
  }
  handleOnPress = () => {this.props.navigation.navigate("AuthStack")}
  static router = PatientTabNavigator.router;
  render() {
    let props = {
      id: this.state.id,
      firstName: this.state.firstName,
      lastName: this.state.lastName,
      handleOnPress: this.handleOnPress
    }
    // pass user info to screens in the patient tab navigator
    return <PatientTabNavigator navigation={this.props.navigation} screenProps={props} />;
  }
}
export default PatientTabNavContainer;
// export default PatientTabNavigator;
