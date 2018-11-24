import * as React from "react";
import settings from "../config/settings";
import {
  Text,
  View,
  StyleSheet,
  AppRegistry,
  RefreshControl,
  ScrollView
} from "react-native";
import { Card, Button } from "react-native-elements"; //0.19.1
import { TouchableOpacity } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import postData from "../lib/postData";
import checkRequestErrors from "../lib/errors";

export default class DoctorInboxScreen extends React.Component {
  acceptOnPress = patientID => {
    const url = settings.REMOTE_SERVER_URL + settings.RELAITON_RES;
    const json = { patient: patientID, doctor: this.props.screenProps.user.id };
    return postData(url, json)
      .then(response => {
        const { email, password } = this.props.screenProps.user;
        const form = { email, password };
        const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
        this.props.screenProps.onSignIn(url, form);
      })
      .catch(error => {
        genAlert("Failed to accept the request", error.message);
      });
  };

  declineOnPress = requestID => {
    console.log(requestID);
    const url =
      settings.REMOTE_SERVER_URL + settings.REQUESTS_RES + "/" + requestID;
    return fetch(url, {
      method: "DELETE",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json"
      }
    })
      .then(checkRequestErrors)
      .then(response => {
        const { email, password } = this.props.screenProps.user;
        const form = { email, password };
        const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
        this.props.screenProps.onSignIn(url, form);
      })
      .catch(error => {
        genAlert("Failed to decline the request", error.message);
      });
  };

  mapRequestToCard = request => (
    <Card key={request.id}>
      <Text style={styles.text}>
        {"Request from " + request.patient.firstName + " " + request.patient.lastName}
      </Text>
      <View style={styles.buttonGroup}>
        <Button
          title="Accept"
          onPress={() => this.acceptOnPress(request.patient.id)}
        />
        <Button
          title="Decline"
          onPress={() => this.declineOnPress(request.id)}
        />
      </View>
    </Card>
  );
  render() {
    const pendingRequests = this.props.screenProps.pendingRequests;
    const requestIDs = this.props.screenProps.user.myPendingRequests;
    console.log(pendingRequests);
    console.log(requestIDs);
    return (
      <View style={styles.container}>
        <ScrollView
          // refreshControl={
          //   <RefreshControl
          //     refreshing={this.props.screenProps.isFetchingUser}
          //     onRefresh={() => {
          //       const { email, password } = this.props.screenProps.user;
          //       const form = { email, password };
          //       const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
          //       this.props.screenProps.onSignIn(url, form);
          //     }}
          // />
          // }
        >
          {requestIDs.map(id =>
            this.mapRequestToCard(pendingRequests.byId[id])
          )}
        </ScrollView>
      </View>
    );
  }
}

AppRegistry.registerComponent("DoctorInboxScreen", () => DoctorInboxScreen);

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#ecf0f1"
  },
  text: {
    fontSize: 20
  },
  buttonGroup: {
    flex: 1,
    flexDirection: "row",
    paddingTop: 10
  }
});
