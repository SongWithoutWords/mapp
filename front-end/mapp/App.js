// this file is similar to index.js in older version react-native
// for more info:
import React, { Component } from "react";
import { Root } from "native-base";
import {AppRegistry} from 'react-native';
import {AuthSwitchNavigator} from "./app/config/router";
import { SafeAreaView } from "react-navigation";

type Props = {};
export default class App extends Component<Props> {
  render() {
    return (
      <Root>
        <SafeAreaView style={{ flex: 1, backgroundColor: "#fff" }}>
          <AuthSwitchNavigator />
        </SafeAreaView>
      </Root>
    );
  }
}

AppRegistry.registerComponent('App', () => App);
