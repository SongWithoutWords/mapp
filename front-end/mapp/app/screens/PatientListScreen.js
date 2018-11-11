import React, { Component } from "react";
import settings from "../config/settings";
import { View, ScrollView, AppRegistry } from "react-native";
import { List, ListItem } from "react-native-elements";
import genAlert from "../components/generalComponents/genAlert";
import getDoctorData from "../lib/getDoctorData";
import PatientPrescriptionList from "../screens/PatientPrescriptionList";
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
    return getDoctorData(this.state.doctorID)
      .then(responseJson => {
        this.setState({
          patients: responseJson.patients
        });
      })
      .catch(error => {
        genAlert("Error", "Failed to fetch patient information");
        console.error(error);
      });
  }

  onPress = (patient) => {
    console.log(patient);
    this.props.navigation.navigate("PatientPrescriptionList", {
      patient: patient,
      doctorID: this.state.doctorID
    });
  };

  render() {
    return (
      <View>
        <ScrollView>
          <List>
            <ListItem
              key={1}
              title={"Patient " + "Sina" + " " + "Saleh"}
              subtitle={"Salam"}
              onPress={this.onPress.bind(this, null)}
            />
            {this.state.patients.map(this.patientToListItem)}
          </List>
        </ScrollView>
      </View>
    );
  }
}

export default PatientListScreen;
AppRegistry.registerComponent('PatientListScreen', () => PatientListScreen);
