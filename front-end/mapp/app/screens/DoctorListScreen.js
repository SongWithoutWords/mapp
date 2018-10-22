import React, { Component } from "react";
import { StyleSheet } from "react-native";

import { View, ScrollView } from "react-native";
import { List, ListItem } from "react-native-elements";

const fake_doctors = [
  {
    name: "Hot dog",
    avatar_url:
      "https://s3.amazonaws.com/uifaces/faces/twitter/ladylexy/128.jpg",
    subtitle: "Vice President"
  },
  {
    name: "Not Hot Dog",
    avatar_url:
      "https://s3.amazonaws.com/uifaces/faces/twitter/adhamdannaway/128.jpg",
    subtitle: "Vice Vice President"
  }
];

class DoctorListScreen extends Component {
  onPress = () => {
    this.props.navigation.navigate("DoctorInfo");
  };
  render() {
    return (
      <View>
        <ScrollView>
          <List>
            {fake_doctors.map((doctor, i) => (
              <ListItem
                key={i}
                roundAvatar
                avatar={{ uri: doctor.avatar_url }}
                title={doctor.name}
                subtitle={doctor.subtitle}
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

export default DoctorListScreen;
