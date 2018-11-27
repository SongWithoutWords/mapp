/** @format */

import { AppRegistry } from "react-native";
import App from "./App";
import { name as appName } from "./app.json";

import { Provider as PaperProvider } from "react-native-paper";
import * as React from "react";
import { Provider } from "react-redux";
import configureStore from "./app/store/configureStore";

// GUI testing
import { Tester, TestHookStore } from "cavy";
import signInSpecs from "../mapp/app/specs/signInSpecs";
import signUpSpecs from "../mapp/app/specs/signUpSpecs";
import requestSpecs from "../mapp/app/specs/requestSpecs";
const testHookStore = new TestHookStore();

const store = configureStore();
const specs = [requestSpecs, signInSpecs, signUpSpecs];
export default function Main() {
  return (
    <Provider store={store}>
      <Tester
        specs={specs}
        store={testHookStore}
        sendReport={true}
        waitTime={1000}
        startDelay={3000}
      >
        <PaperProvider>
          <App />
        </PaperProvider>
      </Tester>
    </Provider>
  );
}

AppRegistry.registerComponent(appName, () => Main);
