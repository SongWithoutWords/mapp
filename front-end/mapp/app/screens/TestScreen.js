import settings from "../config/settings"
import React, { Component } from "react";
import {
  StyleSheet,
  ActivityIndicator,
  Text,
  View,
  TextInput
} from "react-native";
import { Button } from "react-native-elements";

class TestScreen extends Component {
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

  onPress = () => {
    return fetch(settings.REMOTE_SERVER_URL + settings.DOCTOR_RES, {
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
            isLoading: false,
            dataSource: responseJson
          },
          function() {}
        );
      })
      .catch(error => {
        console.error(error);
      });
  };
  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  render() {
    return (
      <View style={styles.container}>
        <View style={styles.layout}>
          <TextInput
            style={{ height: 40 }}
            placeholder="First Name"
            onChangeText={firstName => this.setState({ firstName })}
          />
          <TextInput
            style={{ height: 40 }}
            placeholder="Last Name"
            onChangeText={lastName => this.setState({ lastName })}
          />
          <TextInput
            style={{ height: 40 }}
            placeholder="Email"
            onChangeText={email => this.setState({ email })}
          />
          <TextInput
            style={{ height: 40 }}
            placeholder="Password"
            onChangeText={password => this.setState({ password })}
          />
        </View>
        <View style={styles.button}>
          <Text style={styles.text}>
            {JSON.stringify(this.state.dataSource)}
          </Text>
          <Button title="Post a doctor" onPress={this.onPress} />
        </View>
      </View>
    );
  }
}

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
export default TestScreen;
