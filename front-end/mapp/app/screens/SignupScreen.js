import settings from "../config/settings";
import React, { Component } from "react";
import { StyleSheet, Text, View, Alert } from "react-native";
import genAlert from "../components/testComponents/genAlert";
import genToast from "../components/testComponents/genToast";
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
      confirmPassword: "",
      response: {}
    };
  }

  onSignUp = userTypeString => {
    console.log(userTypeString);
    const { email, password, confirmPassword } = this.state;
    const fields = { email, password, confirmPassword };
    console.log(JSON.stringify(fields));
    const result = validate(fields, constraints);
    console.log(JSON.stringify(result, null, 2));

    if (typeof result !== "undefined") {
      genAlert("Sign up failed", JSON.stringify(result));
    } else if (userTypeString.length == 0) {
      genAlert(
        "Sign up failed",
        "User type is undefined, please go back and retry"
      );
    } else {
      const endpoint =
        userTypeString == "doctor" ? settings.DOCTOR_RES : settings.PATIENT_RES;
      return fetch(settings.REMOTE_SERVER_URL + endpoint, {
        method: "POST",
        headers: {
          Accept: "application/json",
          "Content-Type": "application/json"
        },
        body: JSON.stringify({
          email: this.state.email,
          password: this.state.password,
          firstName: this.state.firstName,
          lastName: this.state.lastName
        })
      })
        .then(response => response.json())
        .then(responseJson => {
          console.log(JSON.stringify(responseJson));
          if (responseJson["errors"]) {
            genAlert("Error", JSON.stringify(responseJson));
          } else {
            this.setState(
              {
                response: responseJson
              },
              function() {
                const tabNav =
                  userTypeString == "doctor" ? "DoctorTab" : "PatientTab";
                genToast("Sign up successfully", "Okay", 3000);
                this.props.navigation.navigate(tabNav, responseJson);
              }
            );
          }
        })
        .catch(error => {
          genAlert("Error", JSON.stringify(this.state.response));
          console.error(error);
        });
    }
  };

  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  render() {
    const { navigation } = this.props;
    const userTypeString = navigation.getParam("userType", "");

    return (
      <View style={styles.container}>
        <Card>
          <FormLabel>Email</FormLabel>
          <FormInput
            placeholder="Email address..."
            onChangeText={email => this.setState({ email })}
          />
          <FormLabel>First Name</FormLabel>
          <FormInput
            placeholder="Firstname..."
            onChangeText={firstName => this.setState({ firstName })}
          />
          <FormLabel>Last Name</FormLabel>
          <FormInput
            placeholder="Lastname..."
            onChangeText={lastName => this.setState({ lastName })}
          />
          <FormLabel>Password</FormLabel>
          <FormInput
            secureTextEntry
            placeholder="Password..."
            onChangeText={password => this.setState({ password })}
          />
          <FormLabel>Confirm Password</FormLabel>
          <FormInput
            secureTextEntry
            placeholder="Confirm Password..."
            onChangeText={confirmPassword => this.setState({ confirmPassword })}
          />
          <Button
            buttonStyle={{ marginTop: 20 }}
            backgroundColor={settings.THEME_COLOR}
            title="SIGN UP"
            onPress={() => this.onSignUp(userTypeString)}
          />
        </Card>
      </View>
    );
  }
}

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
export default SignUpScreen;
