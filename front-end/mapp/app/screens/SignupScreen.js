import React, { Component } from "react";
import {
  StyleSheet,
  ActivityIndicator,
  Text,
  View,
  TextInput,
  Alert
} from "react-native";
import { Button } from "react-native-elements";
import {
  Container,
  Header,
  Content,
  Form,
  Item,
  Input,
  Label
} from "native-base";

class SignupScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      isLoading: true,
      isPressed: false,
      firstName: "",
      lastName: "",
      email: "",
      password: ""
    };
  }

  //   onPress = () => {
  //     this.props.navigation.navigate("Main");
  //   };

  onPress = () => {
    return fetch("http://www.agis-mapp.xyz/patients", {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        email: this.state.email,
        password: this.state.password,
        firstName: this.state.firstName,
        lastName: this.state.lastName
      })
    })
      .then(response => response.json())
      .then(responseJson => {
        this.setState(
          {
            isLoading: false,
            dataSource: responseJson
          },
          function() {
            Alert.alert(
              "Created a patient account",
              JSON.stringify(this.state.dataSource),
              [
                {
                  text: "Cancel",
                  onPress: () => console.log("Cancel Pressed"),
                  style: "cancel"
                },
                { text: "OK", onPress: () => console.log("OK Pressed") }
              ],
              { cancelable: false }
            );
            this.props.navigation.navigate("Main");
          }
        );
      })
      .catch(error => {
        console.error(error);
      });
  };
  // more info about fetch and promise:
  // https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises

  render() {
    return (
      <View style={styles.container}>
        <Form>
          <Item floatingLabel>
            <Label>First Name</Label>
            <Input
              placeholder=""
              onChangeText={firstName => this.setState({ firstName })}
            />
          </Item>
          <Item floatingLabel>
            <Label>Last Name</Label>
            <Input
              placeholder=""
              onChangeText={lastName => this.setState({ lastName })}
            />
          </Item>
          <Item floatingLabel>
            <Label>Email</Label>
            <Input
              placeholder=""
              onChangeText={email => this.setState({ email })}
            />
          </Item>
          <Item floatingLabel>
            <Label>Password</Label>
            <Input
              placeholder=""
              onChangeText={password => this.setState({ password })}
            />
          </Item>
        </Form>
        <View style={styles.button}>
          <Text style={styles.text}>
            {JSON.stringify(this.state.dataSource)}
          </Text>
          <Button title="Sign up" onPress={this.onPress} />
        </View>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1
  },
  layout: {
    flex: 1,
    padding: 10
  },
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
export default SignupScreen;
