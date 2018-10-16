/** @format */

import {AppRegistry} from 'react-native';
import App from './App';
import {name as appName} from './app.json';

import { Provider as PaperProvider } from 'react-native-paper';
import * as React from 'react';

export default function Main() {
  return (
    <PaperProvider>
      <App />
    </PaperProvider>
  );
}

//AppRegistry.registerComponent(appName, () => App);
AppRegistry.registerComponent(appName, () => Main);
