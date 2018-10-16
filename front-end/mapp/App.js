import React, {Component} from 'react';
import {AppRegistry, Text, View, Navigator, StyleSheet} from 'react-native';

import { Button } from 'react-native-paper';
import Header from './app/components/testComponents/header';

type Props = {};
export default class App extends Component<Props> {
  render() {
    return (
      <View>
        <Header/>
      </View>
    );
  }
}

