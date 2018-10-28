import React, { Component } from "react";
import { View, ScrollView, AppRegistry } from "react-native";
import { List, ListItem } from "react-native-elements";
import settings from "../config/settings";
import genAlert from "../components/generalComponents/genAlert";
import checkRequestErrors from "../lib/errors";

class DoctorListScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      doctors: [],
      patientID: -1
    };
  }

  componentDidMount() {
    this.setState(
      {
        patientID: this.props.screenProps.id
      },
      this.fetchDoctorData
    );
  }

  fetchDoctorData() {
    return fetch(settings.REMOTE_SERVER_URL + settings.DOCTOR_RES)
      .then(checkRequestErrors)
      .then(response => response.json())
      .then(responseJson => {
        this.setState({
          doctors: responseJson
        });
      })
      .catch(error => {
        genAlert("Error", "Failed to fetch doctor infomation");
        console.error(error);
      });
  }

  onPress = doctor => {
    console.log(doctor);
    this.props.navigation.navigate("DoctorInfo", {
      doctor: doctor,
      patientID: this.state.patientID
    });
  };

  render() {
    return (
      <View>
        <ScrollView>
          <List>
            {this.state.doctors.map((doctor, i) => (
              <ListItem
                key={i}
                roundAvatar
                avatar={{ uri: doctor.avatar_url }}
                title={"Dr. " + doctor.firstName + " " + doctor.lastName}
                subtitle={doctor.subtitle}
                onPress={this.onPress.bind(this, doctor)}
              />
            ))}
          </List>
        </ScrollView>
      </View>
    );
  }
}

export default DoctorListScreen;
AppRegistry.registerComponent('DoctorListScreen', () => DoctorListScreen);
