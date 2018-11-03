import { Button, Card, FormLabel, FormInput } from "react-native-elements";
import React, { Component } from "react";
import { AppRegistry, StyleSheet } from "react-native";

import { View, Text } from "react-native";
import settings from "../config/settings";

class WelcomeScreen extends Component {

  button = ({ title, userType}) => (
      <Button
        buttonStyle={{ marginTop: 20 }}
        backgroundColor={settings.THEME_COLOR}
        title={title}
        onPress={() => {
          this.props.navigation.navigate("SignUp", {
            userType: userType
          });
        }}
      />
  );

  render() {
    return (
      <View style={styles.buttonContainer}>
        <Card>
          <Text style={styles.title}>Welcome to MAPP</Text>
          <Text style={styles.subtitle}>
            An application for doctors to prescribe medications for their
            patients, and patients to receive notifications
          </Text>
          {this.button({"title":"Patient", "userType": "patient"})}
          {this.button({"title":"Doctor", "userType": "doctor"})}
        </Card>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  buttonContainer: {
    flex: 1,
    flexDirection: "column",
    alignItems: "center",
    justifyContent: "center",
    paddingBottom: 10
  },
  title: {
    backgroundColor: "transparent",
    color: settings.THEME_COLOR,
    fontSize: 30,
    padding: 10
  },
  subtitle: {
    backgroundColor: "transparent",
    color: settings.THEME_COLOR,
    fontSize: 15,
    padding: 10
  }
});

export default WelcomeScreen;
AppRegistry.registerComponent("WelcomeScreen", () => WelcomeScreen);
