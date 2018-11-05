import React, { Component } from "react";
import { StyleSheet, Text, View, ScrollView, AppRegistry } from "react-native";
import { Button, Card, FormLabel, FormInput } from "react-native-elements";
import validate from "validate.js";
import settings from "../config/settings";
import genAlert from "../components/generalComponents/genAlert";

class SignInScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      email: "",
      password: ""
    };
  }
  // TODO: implement signin after integrating redux
  onSignIn = () => {
    const { email, password } = this.state;
    const fields = { email, password };
    const result = validate(fields, constraints);

    if (typeof result !== "undefined") {
      genAlert("Sign in failed", JSON.stringify(result)); // TODO: making the form display errors rather than an alert
    } else {
      const { email, password } = this.state;
      const form = { email, password };
      const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
      this.props.screenProps.onSignIn(url, form);
    }
  };

  formItem = ({ itemLabel, key, isSecureEntry = false }) => (
    <>
      <FormLabel>{itemLabel}</FormLabel>
      <FormInput
        autoCapitalize="none"
        secureTextEntry={isSecureEntry}
        placeholder={itemLabel}
        onChangeText={value => this.setState({ [key]: value })}
      />
    </>
  );

  render() {
    const userTypeString = this.props.navigation.getParam("userType", "");
    return (
      <View style={styles.container}>
        <ScrollView>
          <Card>
            {this.formItem({ itemLabel: "Email", key: "email" })}
            {this.formItem({
              itemLabel: "Password",
              key: "password",
              isSecureEntry: true
            })}
            <Button
              buttonStyle={{ marginTop: 20 }}
              backgroundColor={settings.THEME_COLOR}
              title="SIGN IN"
              onPress={() => this.onSignIn(userTypeString)}
            />
          </Card>
        </ScrollView>
      </View>
    );
  }
}

var constraints = {
  email: {
    email: true
  },
  password: {
    presence: true
  }
};
const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  button: {
    flex: 1,
    flexDirection: "column",
    alignItems: "stretch",
    justifyContent: "flex-end",
    marginTop: 10,
    marginBottom: 10
  }
});

export default SignInScreen;
AppRegistry.registerComponent("SignInScreen", () => SignInScreen);
