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
        <Text style={styles.medfield}>
          Patient Name: {patient.firstName} {patient.lastName}
        </Text>
        <Text style={styles.medfield}>
          Patient ID: {patient.id}
        </Text>
        <Text style={styles.medfield}>
          Date of Birth: {patient.dateOfBirth}
        </Text>
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
    justifyContent: "flex-start"
  },
  medfield: {
    fontSize: 16,
    fontWeight: "600",
    color: "#009CC6",
    margin: 10,
  }
});

export default PatientInfoScreen;
AppRegistry.registerComponent("PatientInfoScreen", () => PatientInfoScreen);
