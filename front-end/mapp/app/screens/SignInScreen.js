import genAlert from "../components/testComponents/genAlert"
import settings from "../config/settings";
import React, { Component } from "react";
import { StyleSheet, Text, View, Alert } from "react-native";
import {
  Button,
  Card,
  FormLabel,
  FormInput,
} from "react-native-elements";

class SignInScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isLoading: true,
      isPressed: false,
      firstName: "",
      lastName: "",
      email: "",
      password: ""
    };
  }
  onSignIn = () => {
    const msg = "Sorry, We haven't implemented user authentication on the server side."
    genAlert(msg);
  }
  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  render() {
    return (
      <View style={styles.container}>
        <View style={styles.card}>
          <Card>
            <FormLabel>Email</FormLabel>
            <FormInput placeholder="Email address..." />
            <FormLabel>Password</FormLabel>
            <FormInput secureTextEntry placeholder="Password..." />
            <Button
              buttonStyle={{ marginTop: 20 }}
              backgroundColor="#694fad"
              title="SIGN IN"
              onPress={() => this.onSignIn()}
            />
            <Button
              buttonStyle={{ marginTop: 20 }}
              backgroundColor="transparent"
              textStyle={{ color: "#bcbec1" }}
              title="SIGN UP"
              onPress={() => this.props.navigation.navigate("SignUp")}
            />
          </Card>
        </View>
      </View>
    );
  }
}

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
  }});
export default SignInScreen;
