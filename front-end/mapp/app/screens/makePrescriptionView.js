import * as React from "react";
import {
  Text,
  View,
  StyleSheet,
  TextInput,
  ScrollView,
  Button,
  TouchableOpacity,
  AppRegistry
} from "react-native";
// or any pure javascript modules available in npm
import { Card, CheckBox } from "react-native-elements"; // 0.19.1
import settings from "../config/settings";
import postData from "../lib/postData";
import genAlert from "../components/generalComponents/genAlert";

export default class MakePrescriptionView extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      medName: "",
      medDoseUnit: "",
      medInitialAmount: 0,
      startDate: new Date(),
      endDate: new Date(),
      medFreq: ""
    };
  }

  createPrescriptionOnPress = () => {
    const patient = this.props.navigation.getParam("patient", {});
    const user = this.props.navigation.getParam("user", {});
    const url = settings.REMOTE_SERVER_URL + settings.PRESCRIPTION_RES;

    const json = {
      patient: patient.id,
      doctor: user.id,
      medication: "Amoxicillin",
      dosageUnit: "Gram",
      amountInitial: 20,
      dosageSchedule: [
        {
          firstDose: new Date(Date.now() + (30 * 1000)),
          minutesBetweenDoses: 1,
          dosage: 0.5
        }
      ]
    };

    return postData(url, json)
      .then(response => {
        genAlert(
          "Form Input handler has not been implemented",
          "but a mock prescription has been created"
        );
        this.props.navigation.goBack();
      })
      .catch(error => {
        genAlert("Failed to create a new prescription", error.message);
      });
  };

  render() {
    return (
      <ScrollView style={{ padding: 20 }}>
        <Text style={styles.formSection}>Medication Information</Text>
        <TextInput
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Medication Name"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ medName: value })}
        />
        <TextInput
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Dosage Unit"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ medDoseUnit: value })}
        />
        <TextInput
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Initial Amount"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ medInitialAmount: value })}
        />
        <TextInput
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Start Date"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ startDate: value })}
        />
        <TextInput
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="End Date"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ endDate: value })}
        />
        <TextInput
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Frequency"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ medFreq: value })}
        />
        <Button
          title="Create a new prescription"
          onPress={this.createPrescriptionOnPress}
        />
      </ScrollView>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
    paddingTop: 10,
    backgroundColor: "#ecf0f1"
  },
  formSection: {
    fontSize: 20,
    padding: 5,
    margin: 15,
    color: "black"
  },
  paragraph: {
    margin: 24,
    fontSize: 18,
    fontWeight: "bold",
    textAlign: "center",
    color: "#009CC6"
  },
  input: {
    margin: 15,
    height: 40,
    padding: 10,
    borderColor: "#009CC6",
    borderWidth: 1,
    borderRadius: 7
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    textAlign: "center",
    color: "black"
  },
  medfield: {
    fontSize: 16,
    fontWeight: "600",
    color: "#009CC6",
    marginBottom: 40,
    marginLeft: 24,
    marginRight: 24
  },
  submitButton: {
    backgroundColor: "#009CC6",
    padding: 10,
    margin: 15,
    height: 40,
    borderRadius: 10,
    alignItems: "center",
    width: "25%"
  },
  submitButtonText: {
    color: "white"
  }
});
AppRegistry.registerComponent(
  "MakePrescriptionView",
  () => MakePrescriptionView
);
