import React, { Component } from "react";
import { StyleSheet, AppRegistry } from "react-native";

import { View, ScrollView } from "react-native";
import { List, ListItem } from "react-native-elements";

const fake_patients = [
  {
    name: "Ursula Carey",
    avatar_url:
      "https://s3.amazonaws.com/uifaces/faces/twitter/ladylexy/128.jpg",
    subtitle: "Cardiology"
  },
  {
    name: "Keeva Mcleod",
    avatar_url:
      "https://s3.amazonaws.com/uifaces/faces/twitter/adhamdannaway/128.jpg",
    subtitle: "Clinical immunology/allergy"
  }
];

class PatientListScreen extends Component {
  onPress = () => {
    this.props.navigation.navigate("PatientInfo");
  };
  render() {
    return (
      <View>
        <ScrollView>
          <List>
            {fake_patients.map((patient, i) => (
              <ListItem
                key={i}
                roundAvatar
                avatar={{ uri: patient.avatar_url }}
                title={patient.name}
                subtitle={patient.subtitle}
                onPress={this.onPress}
              />
            ))}
          </List>
        </ScrollView>
      </View>
    );
  }
}

// styles for this screen
const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center"
  }
});

export default PatientListScreen;
AppRegistry.registerComponent('PatientListScreen', () => PatientListScreen);
