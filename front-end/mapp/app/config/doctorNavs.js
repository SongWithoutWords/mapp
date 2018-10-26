import React from "react";
import settings from "../config/settings";
import { createMaterialBottomTabNavigator } from "react-navigation-material-bottom-tabs";
import MaterialCommunityIcons from "react-native-vector-icons/MaterialCommunityIcons";
import MaterialIcons from "react-native-vector-icons/MaterialIcons";
import { createStackNavigator } from "react-navigation";

import InboxScreen from "../screens/InboxScreen";
import AccountScreen from "../screens/AccountScreen";
import DoctorListScreen from "../screens/DoctorListScreen";
import DoctorInfoScreen from "../screens/DoctorInfoScreen";

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

// tab navigator for doctor
const DoctorTabNavigator = createMaterialBottomTabNavigator(
  {
    // screens and their navigation options
    DoctorList: {
      screen: DoctorStackNavigator,
      navigationOptions: {
        tabBarLabel: "Doctors",
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
    initialRouteName: "DoctorList",
    shifting: true,
    activeColor: settings.ACTIVE_COLOR,
    inactiveColor: settings.INACTIVE_COLOR,
    barStyle: { backgroundColor: settings.THEME_COLOR },
    tabBarOptions: {
      showIcon: true,
      labelStyle: { fontSize: 10 }
    },
    order: ["Inbox", "DoctorList", "Account"]
  }
);

// class DoctorTabNavContainer extends React.Component {
//   constructor(props) {
//     super(props);
//     this.state = {
//       lastName:'',
//       firstName:'',
//       id:'',
//     };
//   }
//   componentDidMount() {
//     const { navigation } = this.props;
//     const firstName = navigation.getParam("firstName", "");
//     const lastName = navigation.getParam("lastName", "");
//     const id = navigation.getParam("id", "");
//     console.log(firstName);
//     console.log(lastName);
//     console.log(id);
//     this.setState({lastName})
//     this.setState({firstName})
//     this.setState({id})
//   }

//   render() {
//     // pass user info to screens in the patient tab navigator
//     return(<DoctorTabNavigator screenProps={this.state} />);
//   }
// }
// export default DoctorTabNavContainer;
export default DoctorTabNavigator;