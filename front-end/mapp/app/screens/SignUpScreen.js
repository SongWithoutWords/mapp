import settings from "../config/settings";
import React, { Component } from "react";
import { StyleSheet, View, AppRegistry } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import validate from "validate.js";
import { Button, Card, FormLabel, FormInput } from "react-native-elements";

class SignUpScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      firstName: "",
      lastName: "",
      email: "",
      password: "",
      confirmPassword: ""
    };
  }

  onSignUp = userTypeString => {
    const { email, password, confirmPassword } = this.state;
    const fields = { email, password, confirmPassword };
    const result = validate(fields, constraints);

    if (typeof result !== "undefined") {
      genAlert("Sign up failed", JSON.stringify(result)); // TODO: making the form display errors rather than an alert
    } else {
      const {
        firstName,
        lastName,
        email,
        password,
      } = this.state;
      const form = { firstName, lastName, email, password};
      const endpoint =
        userTypeString == "doctor" ? settings.DOCTOR_RES : settings.PATIENT_RES;
      const url = settings.REMOTE_SERVER_URL + endpoint;
      this.props.screenProps.onSignIn(url, form);
    }
  };

  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

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
    const { navigation } = this.props;
    const userTypeString = navigation.getParam("userType", "");
    // TODO: tap outside to hide keyboard
    return (
      <View style={styles.container}>
        <Card>
          {this.formItem({ itemLabel: "Email", key: "email" })}
          {this.formItem({ itemLabel: "First Name", key: "firstName" })}
          {this.formItem({ itemLabel: "Last Name", key: "lastName" })}
          {this.formItem({
            itemLabel: "Password",
            key: "password",
            isSecureEntry: true
          })}
          {this.formItem({
            itemLabel: "Confirm Password",
            key: "confirmPassword",
            isSecureEntry: true
          })}
          <Button
            buttonStyle={{ marginTop: 20 }}
            backgroundColor={settings.THEME_COLOR}
            title="SIGN UP"
            onPress={() => this.onSignUp(userTypeString)}
          />
          <Button
            buttonStyle={{ marginTop: 20 }}
            backgroundColor="transparent"
            textStyle={{ color: "#bcbec1" }}
            title="SIGN IN"
            onPress={() =>
              this.props.navigation.navigate("SignIn", {
                userType: userTypeString
              })
            }
          />
        </Card>
      </View>
    );
  }
}

export default SignUpScreen;
AppRegistry.registerComponent("SignUpScreen", () => SignUpScreen);

var constraints = {
  password: {
    presence: true,
    length: {
      minimum: 6,
      message: "must be at least 6 characters"
    }
  },
  confirmPassword: {
    equality: {
      attribute: "password",
      message: "is different from the password"
    }
  },
  email: {
    email: true
  }
};

const styles = StyleSheet.create({
  container: {
    flex: 1
  },
  layout: {
    flex: 1,
    padding: 10
  },
  button: {
    flex: 1,
    flexDirection: "column",
    alignItems: "stretch",
    justifyContent: "flex-end",
    marginTop: 10,
    marginBottom: 10
  },
  text: {
    backgroundColor: "whitesmoke",
    color: "#4A90E2",
    fontSize: 24,
    padding: 10
  }
});
