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
import getDoctorData from "../lib/getDoctorData";

export default class PendingRequestScreen extends React.Component {
  mapRequestToCard = request => (
    <Card key={request.id}>
      <Text style={styles.text}>
        Sent Request to <Text style={styles.DrText}>
        { " Dr. " + request.doctor.firstName +  " " + request.doctor.lastName}
        </Text>
      </Text>
    </Card>
  );

  render() {
    const pendingRequests = this.props.screenProps.pendingRequests;
    const requestIDs = this.props.screenProps.user.myPendingRequests;
    console.log(pendingRequests);
    console.log(requestIDs);
    return (
      <View style={styles.container}>
        <ScrollView>
          {requestIDs.map(id =>
            this.mapRequestToCard(pendingRequests.byId[id])
          )}
        </ScrollView>
      </View>
    );
  }
}

AppRegistry.registerComponent(
  "PendingRequestScreen",
  () => PendingRequestScreen
);

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#ecf0f1"
  },
  text: {
    fontSize: 20,
    fontFamily: 'Poppins-Light'
  },
  DrText: {
    color: settings.THEME_COLOR,
    fontFamily: 'Poppins-Medium',
    fontSize: 20
  },
  buttonGroup: {
    flex: 1,
    flexDirection: "row",
    paddingTop: 10
  }
});
