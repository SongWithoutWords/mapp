import React, { Component } from "react";
import settings from "../config/settings";
import { View, ScrollView, AppRegistry } from "react-native";
import { List, ListItem } from "react-native-elements";
import genAlert from "../components/generalComponents/genAlert";
import getDoctorData from "../lib/getDoctorData";

// note that this class now is actually "my patients" screen
// cuz unlike doctor list screen, patients' info can only accessed
// by their doctors
class PatientListScreen extends Component {

  onPress = id => {
    console.log(id);
    this.props.navigation.navigate("PatientInfo", {
      patient: this.props.screenProps.patients.byId[id],
      user: this.props.screenProps.user
    });
  };

  render() {
    // right now these are patients associated with each doctor
    const myPatients = this.props.screenProps.patients;
    const myPatientIDs = this.props.screenProps.user.myPatients;
    return (
      <View>
        <ScrollView>
          <List>
            {myPatientIDs.map(id => (
              <ListItem
                key={id}
                title={
                  "Patient " +
                  myPatients["byId"][id]["firstName"] +
                  " " +
                  myPatients["byId"][id]["lastName"]
                }
                onPress={() => this.onPress(id)}
              />
            ))}
          </List>
        </ScrollView>
      </View>
    );
  }
}

export default PatientListScreen;
AppRegistry.registerComponent("PatientListScreen", () => PatientListScreen);
