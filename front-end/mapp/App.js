// this file is similar to index.js in older version react-native
// for more info: 
import React, {Component} from 'react';
import {AppRegistry, Text, View, Navigator, StyleSheet} from 'react-native';
import { Tabs } from './app/config/router';


type Props = {};
export default class App extends Component<Props> {
  render() {
    return (
      <Tabs/>
    );
  }
}

