import postData  from "../lib/postData";
import settings from "../config/settings";
import React, { Component } from "react";
import { StyleSheet, View } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import genToast from "../components/generalComponents/genToast";
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
      response:{}
    };
  }

  onSignUp = userTypeString => {
    const { email, password, confirmPassword } = this.state;
    const fields = { email, password, confirmPassword };
    const result = validate(fields, constraints);

    if (typeof result !== "undefined") {
      genAlert("Sign up failed", JSON.stringify(result)); // TODO: making the form display errors rather than an alert
    } else {
      const endpoint =
        userTypeString == "doctor" ? settings.DOCTOR_RES : settings.PATIENT_RES;
      const url = settings.REMOTE_SERVER_URL + endpoint;
      const { email, password, firstName, lastName } = this.state;
      const json = { email, password, firstName, lastName};
        postData(url, json) 
        .then(response => response.json())
        .then(responseJson => {
          if (responseJson["errors"]) {
            genAlert("Error", JSON.stringify(responseJson));
          } else {
            // TODO: invoke redux dispatch to update states based on
            // userType
            this.setState(
              {
                response: responseJson
              },
              function() {
                const tabNav =
                  userTypeString == "doctor" ? "DoctorTab" : "PatientTab";
                genToast("Sign up successfully", "Okay", 2000);
                this.props.navigation.navigate(tabNav, responseJson);                
                // replace params with redux
                // respons json contains current user's information 
                // it can be accessed by child components via screenProps
              }
            );
          }
        })
        .catch(error => {
          genAlert(error.name, error.message);
          // TODO: invoke dispatch displayTheError(errorMessage)
        });
    }
  };

  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  render() {
    const { navigation } = this.props;
    const userTypeString = navigation.getParam("userType", "");

    // TODO: tap outside to hide keyboard
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
