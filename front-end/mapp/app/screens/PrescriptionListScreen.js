import React, { Component } from "react";
import { SafeAreaView } from "react-navigation";

import { Text, View, ScrollView } from "react-native";
import { List, ListItem } from "react-native-elements";
import { Container, Header, Content, Card, CardItem, Body } from "native-base";

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
  render() {
    return (
      <SafeAreaView>
        <List>
          {fake_prescriptions.map((prescription, i) => (
            <ListItem
              key={i}
              roundAvatar
              avatar={{ uri: prescription.avatar_url }}
              title={prescription.name}
              subtitle={prescription.subtitle}
            />
          ))}
        </List>
      </SafeAreaView>
    );
  }
}

export default PrescriptionListScreen;
