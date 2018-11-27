import * as React from "react";
import settings from "../config/settings";
import {
  Text,
  View,
  StyleSheet,
  AppRegistry,
  RefreshControl,
  ScrollView,
} from "react-native";
import { Card, Button } from "react-native-elements"; //0.19.1
import { TouchableOpacity } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import postData from "../lib/postData";
import checkRequestErrors from "../lib/errors";
import fetchAuth from "../lib/fetchAuth";

export default class DoctorInboxScreen extends React.Component {
  acceptOnPress = patientID => {
    const url = settings.REMOTE_SERVER_URL + settings.RELAITON_RES;
    const data = { patient: patientID, doctor: this.props.screenProps.user.id };
    const { email, password } = this.props.screenProps.user;
    return postData(url, data, email, password)
      .then(response => {
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
    const { email, password } = this.props.screenProps.user;
    const method = "DELETE";
    return fetchAuth({ url, method, email, password })
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
      <Text style={styles.fieldValue}>
        You have a new request from
        <Text style={styles.medfield}>
          { " " + request.patient.firstName +
            " " +
            request.patient.lastName}
        </Text>
      </Text>
      <View style={{
        alignItems : 'center',
        justifyContent : 'center',
        flexDirection: 'row',
      }}>
        <View style={{width: '40%'}}>
          <TouchableOpacity title="Accept" onPress={() => this.acceptOnPress(request.patient.id)} style={styles.RenewButton}>
            <Text style = {styles.buttonText}>Accept</Text>
          </TouchableOpacity>
        </View>
        <View style={{width: '40%'}}>
          <TouchableOpacity onPress={() => this.declineOnPress(request.id)} style={styles.EditButton}>
            <Text style = {styles.buttonText}>Decline</Text>
          </TouchableOpacity>
        </View>
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
    fontSize: 20,

  },
  buttonGroup: {
    flex: 1,
    flexDirection: "row",
    paddingTop: 10
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    fontFamily: 'Poppins',
    color: 'black',
    marginLeft: 5,
    fontFamily: 'Poppins-Light'
  },
  medfield: {
    fontSize: 18,
    fontWeight: '600',
    color: '#009CC6',
    marginLeft: 24,
    fontFamily: 'Poppins-Medium'
  },
  buttonText: {
    color : 'white',
    fontSize: 15,
    // fontFamily: 'lineto-circular-pro-medium'
  },
  EditButton: { alignItems: 'center',
    backgroundColor: '#009CC6',padding: 6, borderRadius:10 ,margin: 14,              //display : state.reacted
  },
  RenewButton: { alignItems: 'center',
    backgroundColor: '#50BB75',padding: 6, borderRadius:10,
    //display : state.reacted
  },
});
