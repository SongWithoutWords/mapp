import settings from "../config/settings";
import { Toast } from "native-base";
import React, { Component } from "react";
import { StyleSheet, Text, View, Alert } from "react-native";
import genAlert from "../components/testComponents/genAlert";
import genToast from "../components/testComponents/genToast";
import validate from 'validate.js';
import {
  Button,
  Card,
  FormLabel,
  FormInput,
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
      response: {}
    };
  }

  onSignUp = () => {
    const {email, password, confirmPassword} = this.state;
    const fields = {email, password, confirmPassword};
    console.log(JSON.stringify(fields));
    const result = validate(fields, constraints);  
    console.log(JSON.stringify(result, null, 2));
    
    if(typeof result !== 'undefined'){
      genAlert(JSON.stringify(result));
    }else {
      return fetch(settings.REMOTE_SERVER_URL + settings.PATIENT_RES, {
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
          this.setState(
            {
              response: responseJson
            },
            function() {
              genToast("Sign up successfully", "Okay", 3000);
              this.props.navigation.navigate("Main");
            }
          );
        })
        .catch(error => {
          genAlert(JSON.stringify(this.state.response));
          console.error(error);
        });
    }
  };

  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  render() {
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
            backgroundColor="#694fad"
            title="SIGN UP"
            onPress={() => this.onSignUp()}
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
    equality: "password"
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
