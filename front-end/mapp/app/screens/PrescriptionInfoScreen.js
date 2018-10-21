import React, { Component } from "react";
import { Button, Text, View, StyleSheet } from "react-native";

class PrescriptionInfoScreen extends Component {
  render() {
    return (
      <View style={styles.container}>
        <Text>Detail info about a prescription.</Text>
        <Button
          title="Go back to prescription list"
          onPress={() => this.props.navigation.goBack()}
        />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center"
  }
});

export default PrescriptionInfoScreen;
