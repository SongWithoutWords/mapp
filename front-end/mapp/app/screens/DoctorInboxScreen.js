import * as React from "react";
import settings from "../config/settings";
import { Text, View, StyleSheet, AppRegistry, ScrollView } from "react-native";
import { Card } from "react-native-elements"; //0.19.1
import { TouchableOpacity } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import postData from "../lib/postData";
import getDoctorData from "../lib/getDoctorData";

export default class DoctorInboxScreen extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      showButtons: true,
      text: ""
    };
  }

  // declinePatientRequest = () => {
  //   this.setState({ text: "You decline" });
  // };

  // acceptPatientRequest = requestID => {
  //   console.log(requestID + " " + this.state.doctorID );
  //   return postData(settings.REMOTE_SERVER_URL + settings.RELAITON_RES,
  //     {patient: requestID, doctor: this.state.doctorID})
  //     .then(response => response.json())
  //     .then(responseJson => {
  //       this.setState(
  //         {
  //           response: responseJson
  //         }
  //       );
  //     })
  //     .catch(error => {
  //       genAlert(error.name, error.message);
  //     });
  // };

  // mapRequestToCard = request => (
  //   <Card key={request.id}>
  //     <Text style={styles.medfield}>
  //       {request.firstName + request.lastName}
  //     </Text>
  //     <View style={styles.acceptRequest}>
  //       {this.state.showButtons && (
  //         <View style={{ width: "40%" }}>
  //           <TouchableOpacity
  //             // onPress={this.acceptPatientRequest.bind(this, request.id)}
  //             style={styles.RenewButton}
  //           >
  //             <Text>Accept</Text>
  //           </TouchableOpacity>
  //         </View>
  //       )}
  //       {this.state.showButtons && (
  //         <View style={{ width: "40%" }}>
  //           <TouchableOpacity
  //             // onPress={this.declinePatientRequest}
  //             style={styles.EditButton}
  //           >
  //             <Text>Decline</Text>
  //           </TouchableOpacity>
  //         </View>
  //       )}
  //     </View>
  //   </Card>
  // );

  mapRequestToCard = request => (
    <Card key={request.id}>
      <Text>{request.firstName + " " + request.lastName}</Text>
      <View>
        {this.state.showButtons && (
          <View>
            <TouchableOpacity
              // onPress={this.acceptPatientRequest.bind(this, request.id)}
            >
              <Text>Accept</Text>
            </TouchableOpacity>
          </View>
        )}
        {this.state.showButtons && (
          <View>
            <TouchableOpacity
            // onPress={this.declinePatientRequest}
            >
              <Text>Decline</Text>
            </TouchableOpacity>
          </View>
        )}
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
        <ScrollView>
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
  }
});