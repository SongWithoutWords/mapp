/** @format */

import { AppRegistry } from "react-native";
import App from "./App";
import { name as appName } from "./app.json";

import { Provider as PaperProvider } from "react-native-paper";
import * as React from "react";
import { Provider } from "react-redux";
import configureStore from './app/store/configureStore'

const store = configureStore();
export default function Main() {
  return (
    <Provider store={store}>
      <PaperProvider>
        <App />
      </PaperProvider>
    </Provider>
  );
}

AppRegistry.registerComponent(appName, () => Main);
