import React, { Component } from "react";
import { Button, Text, View, StyleSheet, TouchableOpacity, Alert, ToastAndroid} from "react-native";

class DoctorInfoScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      buttonText: "Send Request"
    };
  }


  requestDoctor = () => {
    return fetch("http://www.agis-mapp.xyz/requests", {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        doctor: this.props.doctorID,
        patient: this.props.patientID, 
      }),
    })
    .then((responseJson) => {
      console.log(responseJson);
      this.setState({
        buttonText: "Request Sent!"
      });
    })
    .catch((error) => {
      console.error("Couldn't send request " + error);
    });
    
  }

  render() {
    return (
      <View style={styles.container}>
        <Text style={styles.text} > Dr. {this.state.doctorFirstName} {this.state.doctorLastName}</Text>
        <Text style={styles.text} > ID: {this.state.doctorID}</Text>
        <Button
          title="Go back to doctor list"
          onPress={() => this.props.navigation.goBack()}
        />
        <TouchableOpacity style={styles.submitButton} onPress={this.requestDoctor}>
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
    fontWeight: 'bold',
    fontSize: 35,
  }
});

export default DoctorInfoScreen;
