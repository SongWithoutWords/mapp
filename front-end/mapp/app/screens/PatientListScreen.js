import React, { Component } from "react";
import settings from "../config/settings";
import checkRequestErrors from "../lib/errors";
import { View, ScrollView, AppRegistry } from "react-native";
import { List, ListItem } from "react-native-elements";
import genAlert from "../components/generalComponents/genAlert";

class PatientListScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      doctorID: this.props.screenProps.user.id,
      patients: []
    };
  }

  patientToListItem = (patient, i) => (
    <ListItem
      key={i}
      title={"Patient " + patient.firstName + " " + patient.lastName}
      subtitle={patient.subtitle}
      onPress={this.onPress.bind(this, patient)}
    />
  )

  componentDidMount() {
    console.log(this.props.screenProps.user.id);
    this.fetchPatientData();
  }

  fetchPatientData = () => {
    console.log(
      settings.REMOTE_SERVER_URL +
        settings.DOCTOR_RES +
        "/" +
        this.state.doctorID
    );
    return fetch(
      settings.REMOTE_SERVER_URL +
        settings.DOCTOR_RES +
        "/" +
        this.state.doctorID
    )
      .then(checkRequestErrors)
      .then(response => response.json())
      .then(responseJson => {
        this.setState({
          patients: responseJson.patients
        });
      })
      .catch(error => {
        genAlert("Error", "Failed to fetch patient infomation");
        console.error(error);
      });
  }

  onPress = (patient) => {
    console.log(patient);
    this.props.navigation.navigate("PatientInfo", {
      patient: patient,
      doctorID: this.state.doctorID
    });
  };

  render() {
    return (
      <View>
        <ScrollView>
          <List>
            {this.state.patients.map(this.patientToListItem)}
          </List>
        </ScrollView>
      </View>
    );
  }
}

export default PatientListScreen;
AppRegistry.registerComponent('PatientListScreen', () => PatientListScreen);
