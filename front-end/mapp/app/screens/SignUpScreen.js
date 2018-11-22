import settings from "../config/settings";
import { USER_TYPE } from "../config/constants";
import React, { Component } from "react";
import { StyleSheet, Text, View, ScrollView, AppRegistry } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import validate from "validate.js";
import {
  Button,
  Card,
  FormLabel,
  FormInput,
  FormValidationMessage
} from "react-native-elements";

class SignUpScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      firstName: "",
      lastName: "",
      email: "",
      password: "",
      confirmPassword: "",
      firstNameValidateMsg: null,
      lastNameValidateMsg: null,
      emailValidateMsg: null,
      passwordValidateMsg: null,
      confirmPasswordValidateMsg: null
    };
  }

  onSignUp = userTypeString => {
    const {
      email,
      password,
      confirmPassword,
      firstName,
      lastName
    } = this.state;
    const fields = { email, password, confirmPassword, firstName, lastName };
    const result = validate(fields, constraints);

    if (typeof result !== "undefined") {
      const newState = {
        firstNameValidateMsg: null,
        lastNameValidateMsg: null,
        emailValidateMsg: null,
        passwordValidateMsg: null,
        confirmPasswordValidateMsg: null
      };
      Object.keys(result).forEach(
        function(key) {
          msg = result[key].join(", ");
          key = key + "ValidateMsg";
          newState[key] = msg;
        }
      );
      this.setState(newState);

    } else {
      const { firstName, lastName, email, password } = this.state;
      const form = { firstName, lastName, email, password };
      const endpoint =
        userTypeString == USER_TYPE.DOCTOR
          ? settings.DOCTOR_RES
          : settings.PATIENT_RES;
      const url = settings.REMOTE_SERVER_URL + endpoint;
      this.props.screenProps.onSignIn(url, form);
    }
  };

  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  formItem = ({
    itemLabel,
    key,
    isSecureEntry = false,
    validationMsg = null
  }) => {
    return (
      <>
        <FormLabel>{itemLabel}</FormLabel>
        <FormInput
          autoCapitalize="none"
          secureTextEntry={isSecureEntry}
          placeholder={itemLabel}
          onChangeText={value => this.setState({ [key]: value })}
        />
        <FormValidationMessage>{validationMsg}</FormValidationMessage>
      </>
    );
  };

  render() {
    const userTypeString = this.props.navigation.getParam("userType", "");
    return (
      <View style={styles.container}>
        <ScrollView>
          <Card>
            {this.formItem({
              itemLabel: "Email",
              key: "email",
              validationMsg: this.state.emailValidateMsg
            })}
            {this.formItem({
              itemLabel: "First Name",
              key: "firstName",
              validationMsg: this.state.firstNameValidateMsg
            })}
            {this.formItem({
              itemLabel: "Last Name",
              key: "lastName",
              validationMsg: this.state.lastNameValidateMsg
            })}
            {this.formItem({
              itemLabel: "Password",
              key: "password",
              isSecureEntry: true,
              validationMsg: this.state.passwordValidateMsg
            })}
            {this.formItem({
              itemLabel: "Confirm Password",
              key: "confirmPassword",
              isSecureEntry: true,
              validationMsg: this.state.confirmPasswordValidateMsg
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
        </ScrollView>
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
    email: {
      message: "doesn't look like a valid email"
    }
  },
  firstName: {
    format: {
      pattern: "[a-zA-Z]+",
      flags: "i",
      message: "can only contain a-z and A-Z"
    }
  },
  lastName: {
    format: {
      pattern: "[a-zA-Z]+",
      flags: "i",
      message: "can only contain a-z and A-Z"
    }
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
