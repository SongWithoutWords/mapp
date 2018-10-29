// this file is similar to index.js in older version react-native
// for more info:
import React, { Component } from "react";
import { Root } from "native-base";
import { AppRegistry } from "react-native";
import { SafeAreaView } from "react-navigation";
import AuthStackNavigator from "./app/config/authNavs";
import PatientTabNavigator from "./app/config/patientNavs";
import DoctorTabNavigator from "./app/config/doctorNavs";

type Props = {};
export default class App extends Component<Props> {
  constructor(props) {
    super(props);
    this.onSignIn = this.onSignIn.bind(this);
    this.onSignOut = this.onSignOut.bind(this);
    this.state = {
      user: {},
      isLoggedIn: false
    };
  }

  onSignIn(userInfo){
    this.setState({
      user: userInfo,
      isLoggedIn: true
    });
  }

  onSignOut(){
    this.setState({
      user: {},
      isLoggedIn: false
    });
  }

  render() {
    const isLoggedIn = this.state.isLoggedIn;
    const user = this.state.user;
    let navigator;
    if (isLoggedIn) {
      if(user.userType === "doctor"){
        navigator = <DoctorTabNavigator screenProps={{"user": user, "onSignOut":this.onSignOut}}/>;
      }else{
        navigator = <PatientTabNavigator screenProps={{"user": user, "onSignOut":this.onSignOut}}/>;
      }
    } else {
      navigator = <AuthStackNavigator screenProps={{"onSignIn": this.onSignIn}}/>;
    }

    return (
      <Root>
        <SafeAreaView style={{ flex: 1, backgroundColor: "#fff" }}>
          {navigator}
        </SafeAreaView>
      </Root>
    );
  }
}

AppRegistry.registerComponent("App", () => App);
