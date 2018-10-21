import React, { Component } from "react";
import { StyleSheet, ActivityIndicator, Text, View } from "react-native";
import { Button } from "react-native-elements";

class TestScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isLoading: true,
      isPressed: false
    };
  }

  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
  onPress = () => {
    return fetch("https://facebook.github.io/react-native/movies.json")
      .then(response => response.json())
      .then(responseJson => {
        this.setState(
          {
            isLoading: false,
            dataSource: responseJson
          },
          function() {}
        );
      })
      .catch(error => {
        console.error(error);
      });
  };

  render() {
    if (this.state.isLoading && this.state.isPressed) {
      return (
        <View style={{ flex: 1, padding: 20 }}>
          <ActivityIndicator />
        </View>
      );
    }
    return (
      <View style={styles.button}>
        <Text style={styles.text}>{JSON.stringify(this.state.dataSource)}</Text>
        <Button title="Make a request" onPress={this.onPress} />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  button: {
    flex: 1,
    flexDirection: "column",
    alignItems: "stretch",
    justifyContent: "flex-end",
    marginTop: 10,
    marginBottom: 10
  },
  text: {
    backgroundColor: "whitesmoke",
    color: "#4A90E2",
    fontSize: 24,
    padding: 10
  }
});
export default TestScreen;
