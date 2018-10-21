// this file is similar to index.js in older version react-native
// for more info: 
import React, {Component} from 'react';
import { Tabs } from './app/config/router';
import { SafeAreaView } from "react-navigation";

type Props = {};
export default class App extends Component<Props> {
  render() {
    return (
      <SafeAreaView style={{flex: 1, backgroundColor: '#fff'}}>
      <Tabs/>
      </SafeAreaView>
    );
  }
}

