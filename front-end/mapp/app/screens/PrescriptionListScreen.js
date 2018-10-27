import React, { Component } from "react";
import { View, StyleSheet, ScrollView } from "react-native";
import PrescriptionCardComponent from "../components/cardComponents/PrescriptionCardComponent";

const fake_prescriptions = [
  {
    name: "divalproex",
    avatar_url: "https://bit.ly/2yhPJY0",
    subtitle: "..",
    image_name: "divalproex" // index into the images array in the card component
  },
  {
    name: "amoxicillin",
    avatar_url: "https://bit.ly/2yhPJY0",
    subtitle: "..",
    image_name: "amoxicillin"
  },
  {
    name: "divalproex",
    avatar_url: "https://bit.ly/2yhPJY0",
    subtitle: "..",
    image_name: "divalproex" // index into the images array in the card component
  },
  {
    name: "amoxicillin",
    avatar_url: "https://bit.ly/2yhPJY0",
    subtitle: "..",
    image_name: "amoxicillin"
  },
];

class PrescriptionListScreen extends Component {
  onPress = () => {
    this.props.navigation.navigate("PrescriptionInfo");
  };

  render() {
    return (
      <View>
        <ScrollView>
          {fake_prescriptions.map((prescription,i) => {
            return (
              <PrescriptionCardComponent
                title={prescription.name}
                subtitle={prescription.subtitle}
                image_name={prescription.image_name}
                onPress={this.onPress}
                key={i}
              />
            );
          })}
        </ScrollView>
      </View>
    );
  }
}


export default PrescriptionListScreen;
