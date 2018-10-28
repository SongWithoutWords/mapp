import React, { Component } from "react";
import { StyleSheet, AppRegistry } from "react-native";

import { View, ScrollView } from "react-native";
import { List, ListItem } from "react-native-elements";

class DoctorListScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      doctors : []
    };
  }

  componentDidMount(){
    this.fetchDoctorData();
  }

  fetchDoctorData(){
    return fetch('http://www.agis-mapp.xyz/doctors')
    .then((response) => response.json())
    .then((responseJson) => {
      this.setState({
        doctors: responseJson
      });
      console.log(this.state.doctors);
    })
    .catch((error) => {
      console.error(error);
    });
  }

  onPress = () => {
    this.props.navigation.navigate("DoctorInfo");
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
                title= {"Dr. " + doctor.firstName + " " + doctor.lastName}
                subtitle={doctor.subtitle}
                onPress={(doctor) => { //this.props.navigation.navigate("DoctorInfo", doctor.id)
                console.log(doctor.id)}}
              />
            ))}
          </List>
        </ScrollView>
      </View>
    );
  }
}

// // styles for this screen
// const styles = StyleSheet.create({
//   container: {
//     flex: 1,
//     justifyContent: "center",
//     alignItems: "center"
//   }
// });

export default DoctorListScreen;
AppRegistry.registerComponent('DoctorListScreen', () => DoctorListScreen);
