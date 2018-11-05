/** @format */

import { AppRegistry } from "react-native";
import App from "./App";
import { name as appName } from "./app.json";
import { Tester, TestHookStore } from "cavy";
import DoctorListSpec from "../mapp/app/specs/DoctorListSpec";
import SignUpSpec from "../mapp/app/specs/SignUpSpec";

import { Provider as PaperProvider } from "react-native-paper";
import * as React from "react";

const testHookStore = new TestHookStore();

export default function Main() {
  return (
    <Tester
      specs={[DoctorListSpec, SignUpSpec]}
      store={testHookStore}
      waitTime={1000}
      startDelay={3000}
    >
      <PaperProvider>
        <App />
      </PaperProvider>
    </Tester>
  );
}

AppRegistry.registerComponent(appName, () => Main);
