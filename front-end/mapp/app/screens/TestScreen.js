import React, { Component } from "react";
import { Text, View, StyleSheet } from "react-native";

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

class TestScreen extends Component {
  constructor(props) {
    super(props);
    this.state = { isLoading: true };
  }

  render() {
    return <View />;
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

export default TestScreen;
