import React, { Component } from "react";
import { Button, Text, View, StyleSheet, TouchableOpacity } from "react-native";

class DoctorInfoScreen extends Component {
  constructor(props) {
    super(props);
  }

  // onPress = () => {
  //   return fetch("https://www.agis-mapp.xyz/requests", {
  //     method: "POST",
  //     headers: {
  //       Accept: "application/json",
  //       "Content-Type": "application/json"
  //     },
  //     body: JSON.stringify({
  //       doctor: 1,
  //       patient: 1
  //     })
  //   }).catch(error => {
  //     console.error("sdfs" + error);
  //   });
  //   // .then(response => response.json())
  //   // .then(responseJson => {
  //   //   this.setState(
  //   //     {
  //   //       dataSource: responseJson
  //   //     },
  //   //     function() {}
  //   //   );
  //   // })
  // };
  render() {
    return (
      <View style={styles.container}>
        <Text>Detail info about a doctor.</Text>
        <Button
          title="Go back to doctor list"
          onPress={() => this.props.navigation.goBack()}
        />
        <TouchableOpacity style={styles.submitButton}>
          <Text style={styles.submitButtonText}> Request Doctor </Text>
        </TouchableOpacity>
        {/* <Text> {JSON.stringify(this.state.dataSource)}</Text> */}
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center"
  },
  submitButton: {
    backgroundColor: "#009CC6",
    padding: 10,
    margin: 15,
    height: 40,
    borderRadius: 10,
    alignItems: "center",
    width: "45%"
  },
  submitButtonText: {
    color: "white"
  }
});

export default DoctorInfoScreen;
