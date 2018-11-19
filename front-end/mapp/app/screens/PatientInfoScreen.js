import React, { Component } from "react";
import {
  Button,
  Text,
  View,
  StyleSheet,
  TouchableOpacity,
  AppRegistry
} from "react-native";

class PatientInfoScreen extends Component {
  render() {
    const patient = this.props.navigation.getParam("patient", {});
    const user = this.props.navigation.getParam("user", {});
    return (
      <View style={styles.container}>
        <Text>Detail info about a patient.</Text>
        <Text>{patient.id}</Text>
        <Text>{patient.firstName}</Text>
        <Text>{patient.lastName}</Text>
        <Button
          title="Go back to patient list"
          onPress={() => this.props.navigation.goBack()}
        />
        <Button
          title="Create a new prescription"
          onPress={() =>
            this.props.navigation.navigate("MakePrescription", {
              patient: patient,
              user: user
            })
          }
        />
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
  }
});

export default PatientInfoScreen;
AppRegistry.registerComponent("PatientInfoScreen", () => PatientInfoScreen);
