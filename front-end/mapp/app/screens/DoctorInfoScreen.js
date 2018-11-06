import React, { Component } from "react";
import {
  Button,
  Text,
  View,
  StyleSheet,
  TouchableOpacity,
  AppRegistry
} from "react-native";
import settings from "../config/settings";
import { CLEAR_USER } from "../config/constants";
import postData from "../lib/postData";

class DoctorInfoScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      buttonText: "Send Request"
    };
  }

  requestDoctor = () => {
    const user = this.props.navigation.getParam("user", {});
    const doctor = this.props.navigation.getParam("doctor", {});
    console.log(doctor);
    console.log(user);
    const url = settings.REMOTE_SERVER_URL + settings.REQUESTS_RES;
    const json = { doctor: doctor.id, patient: user.id };

    return postData(url, json)
      .then(responseJson => {
        console.log(responseJson);
        this.setState({
          buttonText: "Request Sent!"
        });
      })
      .catch(error => {
        genAlert("Failed to send the request", error.message);
      });
  };

  render() {
    const doctor = this.props.navigation.getParam("doctor", {});
    return (
      <View style={styles.container}>
        <Text style={styles.text}>
          {" "}
          Dr. {doctor.firstName} {doctor.lastName}
        </Text>
        <Text style={styles.text}> ID: {doctor.id}</Text>
        <Button
          title="Go back to doctor list"
          onPress={() => this.props.navigation.goBack()}
        />
        <TouchableOpacity
          style={styles.submitButton}
          onPress={this.requestDoctor}
        >
          <Text style={styles.submitButtonText}> {this.state.buttonText} </Text>
        </TouchableOpacity>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center"
  },
  submitButton: {
    backgroundColor: "#009CC6",
    padding: 10,
    margin: 15,
    height: 40,
    borderRadius: 10,
    alignItems: "center",
    width: "45%"
  },
  submitButtonText: {
    color: "white"
  },
  text: {
    color: "#009CC6",
    fontWeight: "bold",
    fontSize: 35
  }
});

export default DoctorInfoScreen;
AppRegistry.registerComponent("DoctorInfoScreen", () => DoctorInfoScreen);
