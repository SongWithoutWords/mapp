import React, { Component } from "react";
import { StyleSheet, Text, View, Alert } from "react-native";
import { Button, Card, FormLabel, FormInput } from "react-native-elements";
import validate from "validate.js";
import settings from "../config/settings";
import genToast from "../components/generalComponents/genToast";
import genAlert from "../components/generalComponents/genAlert";
import postData from "../lib/postData";

class SignInScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      email: "",
      password: "",
      response: {}
    };
  }
  // TODO: implement signin after integrating redux
  onSignIn = userTypeString => {
    const { email, password } = this.state;
    const fields = { email, password };
    const result = validate(fields, constraints);

    if (typeof result !== "undefined") {
      genAlert("Sign in failed", JSON.stringify(result)); // TODO: making the form display errors rather than an alert
    } else {
      const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
      const { email, password } = this.state;
      const json = { email, password };
      postData(url, json)
        .then(response => response.json())
        .then(responseJson => {
          // TODO: invoke redux dispatch to update states based on
          // userType
          const isPatient = responseJson.hasOwnProperty("doctors");
          const expectedUserType = isPatient ? "patient" : "doctor";
          if (expectedUserType !== userTypeString) {
            genAlert(
              "Sign in failed",
              "Email or password is incorrect, please retry"
            ); // TODO: making the form display errors rather than an alert
          } else {
            this.setState(
              {
                response: responseJson
              },
              function() {
                const tabNav =
                  userTypeString == "doctor" ? "DoctorTab" : "PatientTab";
                genToast("Sign in successfully", "Okay", 2000);
                this.props.navigation.navigate(tabNav, responseJson);
                // replace params with redux
              }
            );
          }
        })
        .catch(error => {
          genAlert("Sign in failed", error.message);
          // TODO: invoke dispatch displayTheError(errorMessage)
        });
    }
  };

  render() {
    const { navigation } = this.props;
    const userTypeString = navigation.getParam("userType", "");
    return (
      <View style={styles.container}>
        <View style={styles.card}>
          <Card>
            <FormLabel>Email</FormLabel>
            <FormInput
              placeholder="Email address..."
              onChangeText={email => this.setState({ email })}
            />
            <FormLabel>Password</FormLabel>
            <FormInput
              secureTextEntry
              placeholder="Password..."
              onChangeText={password => this.setState({ password })}
            />
            <Button
              buttonStyle={{ marginTop: 20 }}
              backgroundColor="#694fad"
              title="SIGN IN"
              onPress={() => this.onSignIn(userTypeString)}
            />
            <Button
              buttonStyle={{ marginTop: 20 }}
              backgroundColor="transparent"
              textStyle={{ color: "#bcbec1" }}
              title="SIGN UP"
              onPress={() =>
                this.props.navigation.navigate("SignUp", {
                  userType: userTypeString
                })
              }
            />
          </Card>
        </View>
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
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center"
  },
  card: {
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
  }
});
export default SignInScreen;
