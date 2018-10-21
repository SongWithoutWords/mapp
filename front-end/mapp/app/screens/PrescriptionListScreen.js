import React, { Component } from "react";
import { List, ListItem } from "react-native-elements";
import { View, StyleSheet } from "react-native";

const fake_prescriptions = [
  {
    name: "divalproex",
    avatar_url: "https://bit.ly/2yhPJY0",
    subtitle: ".."
  },
  {
    name: "divalproex",
    avatar_url: "https://bit.ly/2yhPJY0",
    subtitle: ".."
  }
];

class PrescriptionListScreen extends Component {

  onPress = () => {
    this.props.navigation.navigate("PrescriptionInfo");
  };

  render() {
    return (
      <View>
        <List>
          {fake_prescriptions.map((prescription, i) => (
            <ListItem
              key={i}
              roundAvatar
              avatar={{ uri: prescription.avatar_url }}
              title={prescription.name}
              subtitle={prescription.subtitle}
              onPress={this.onPress}
            />
          ))}
        </List>
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

export default PrescriptionListScreen;
