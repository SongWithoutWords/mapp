import React, { Component } from "react";
import { Button, Text, View, StyleSheet, AppRegistry } from "react-native";

class PrescriptionInfoScreen extends Component {
  render() {
    const prescription = this.props.navigation.getParam("prescription", {});
    return (
      <View style={styles.container}>
        <Text>Detail info about a prescription.</Text>
        <Text>{JSON.stringify(prescription)}</Text>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: "center",
    margin: 10,
  }
});

export default PrescriptionInfoScreen;
AppRegistry.registerComponent('PrescriptionInfoScreen', () => PrescriptionInfoScreen);
