import React, { Component } from "react";
import {
  Text,
  Image,
  StyleSheet,
  View,
  TouchableWithoutFeedback,
  AppRegistry
} from "react-native";

import { Card, CardItem, Body, Left, Button, Icon } from "native-base";
import { hook, wrap } from "cavy";

const images = {
  divalproex: require("../../assets/images/divalproex.jpg"),
  amoxicillin: require("../../assets/images/amoxicillin.png")
};

class PrescriptionCardComponent extends Component {
  constructor(props) {
    super(props);
  }
  render() {
    return (
      <TouchableWithoutFeedback onPress={this.props.onPress}>
        <Card>
          <CardItem header bordered>
            <Text style={styles.text}>{this.props.title}</Text>
          </CardItem>
          <CardItem bordered>
            <CardItem cardBody>
              <Image source={images[this.props.image_name]} style={{ height: 125, width: null, flex: 1 }}/>
            </CardItem>
          </CardItem>
          <CardItem bordered>
            <Body>
              <View>
                <Text>Frequency: --</Text>
                <Text>Physician: --</Text>
                <Text>Location: --</Text>
              </View>
            </Body>
          </CardItem>
        </Card>
      </TouchableWithoutFeedback>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center"
  },
  text: {
    color: "#694fad",
    fontSize: 20
  }
});

export default hook(PrescriptionCardComponent);
AppRegistry.registerComponent(
  "PrescriptionCardComponent",
  () => PrescriptionCardComponent
);
