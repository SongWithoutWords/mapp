import React, { Component } from "react";
import settings from "../config/settings";
import {
  Button,
  Text} from "react-native-elements";
import { StyleSheet } from "react-native";
import { View } from "react-native";

class AccountScreen extends Component {

  render() {
    const firstName = this.props.screenProps.firstName;
    const lastName = this.props.screenProps.lastName;
    const id = this.props.screenProps.id;

    return (
      <View style={styles.buttonContainer}>
        <Text style={styles.subtitle}>
          {firstName} {lastName}
        </Text>
        <Text style={styles.subtitle}>id: {id}</Text>
        <Button
          buttonStyle={styles.bottomButton}
          backgroundColor={settings.THEME_COLOR}
          title="SIGN OUT"
          onPress={() => this.props.screenProps.handleOnPress()}
        />
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
  subtitle: {
    backgroundColor: "transparent",
    color: settings.THEME_COLOR,
    fontSize: 20,
    padding: 10
  }
});

export default AccountScreen;
