import React, { Component } from "react";
import { View, ScrollView, AppRegistry } from "react-native";
import { List, ListItem } from "react-native-elements";
import settings from "../config/settings";
import genAlert from "../components/generalComponents/genAlert";
import checkRequestErrors from "../lib/errors";
import { FETCHING_USER_FULFILLED } from "../config/constants";

class DoctorListScreen extends Component {
  componentWillMount() {
    this.props.screenProps.fetchDoctors();
  } 

  onPress = id => {
    console.log(id);
    this.props.navigation.navigate("DoctorInfo", {
      doctor: this.props.screenProps.doctors.byId[id],
      user: this.props.screenProps.user
    });
  };

  render() {
    const doctors = this.props.screenProps.doctors;
    console.log("Doctors: " + JSON.stringify(doctors))
    return (
      <View>
        <ScrollView>
          <List>
            {doctors.allIds.map(id => (
              <ListItem
                key={id}
                title={
                  "Dr. " +
                  doctors['byId'][id]['firstName'] +
                  " " +
                  doctors['byId'][id]['lastName']
                }
                onPress={()=>this.onPress(id)}
              />
            ))}
          </List>
        </ScrollView>
      </View>
    );
  }
}

export default DoctorListScreen;
AppRegistry.registerComponent("DoctorListScreen", () => DoctorListScreen);

// {/* {doctors.map(doctor => (
//   <ListItem
//     key={doctor.id}
//     roundAvatar
//     avatar={{ uri: doctor.avatar_url }}
//     title={"Dr. " + doctor.firstName + " " + doctor.lastName}
//     subtitle={doctor.subtitle}
//     onPress={this.onPress.bind(this, doctor)}
//   />
// ))} */}
