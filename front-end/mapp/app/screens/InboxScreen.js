import * as React from "react";
import settings from "../config/settings";
import { Text, View, StyleSheet, AppRegistry, ScrollView } from "react-native";
import { Card } from "react-native-elements"; //0.19.1
import { TouchableOpacity } from "react-native";
import genAlert from "../components/generalComponents/genAlert";
import postData from "../lib/postData";
import getDoctorData from "../lib/getDoctorData";

export default class InboxScreen extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      showButtons: true,
      text: "",
      doctorID: 10,
      patientID: -1,
      pendingRequests: []
    };

    this.acceptPatientRequest = this.acceptPatientRequest.bind(this);
  }

  componentDidMount() {
    this.fetchDoctorData();
    this.text = "you have new requests";
  }

  declinePatientRequest = () => {
    this.setState({ text: "You decline" });
  };

  acceptPatientRequest = requestID => {
    console.log(requestID + " " + this.state.doctorID );
    return postData(settings.REMOTE_SERVER_URL + settings.RELAITON_RES, 
      {patient: requestID, doctor: this.state.doctorID})
      .then(response => response.json())
      .then(responseJson => {
        this.setState(
          {
            response: responseJson
          }
        );
      })
      .catch(error => {
        genAlert(error.name, error.message);
      });
  };

  mapRequestToCard = (request, i) => (
        <Card>
        <Text style={styles.medfield}>
          <Text style={styles.fieldValue}>
            {this.state.text}
          </Text> {request.firstName + request.lastName}
        </Text>
        <View style={styles.acceptRequest}>

        {this.state.showButtons && <View style={{width: '40%'}}>
        <TouchableOpacity onPress={this.acceptPatientRequest.bind(this, request.id)} style={styles.RenewButton}>
          <Text style = {{color : 'white', fontFamily: 'Circular', fontWeight:'500', fontSize: 16}}>Accept</Text>
        </TouchableOpacity>
        </View>}
        {this.state.showButtons && <View style={{width: '40%'}}>
        <TouchableOpacity onPress={this.declinePatientRequest} style={styles.EditButton}>
          <Text style = {{color : 'white', fontFamily: 'Circular', fontWeight:'500', fontSize: 16}}>Decline</Text>
        </TouchableOpacity>
        </View>}
        </View>
        </Card>
      )


  fetchDoctorData() {
    console.log( "fetchDoctorData: " + this.state.doctorID );
    return getDoctorData(this.state.doctorID)
      .then(responseJson => {
        this.setState({
          pendingRequests: responseJson.pendingRequests
        });
      })
      .catch(error => {
        genAlert(error.name, error.message);
      });
  }


  render() {
    return (
      <View>
      <ScrollView>
      <View style={styles.container}>
        {this.state.pendingRequests.map(this.mapRequestToCard)}
        <TouchableOpacity
          onPress={this.fetchDoctorData.bind(this)}
          style={styles.button}
        >
          <Text
            style={{
              color: "white",
              fontWeight: "500",
              fontSize: 16
            }}
          >
            Refresh
          </Text>
        </TouchableOpacity>
      </View>
      </ScrollView>
      </View>
    );
  }
}

AppRegistry.registerComponent('InboxScreen', () => InboxScreen);

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: "center",
    //justifyContent: 'center',
    //paddingTop: Constants.statusBarHeight,
    backgroundColor: "#ecf0f1"
  },
  acceptRequest: {
    alignItems : 'center',
    justifyContent : 'center',
    flexDirection: 'row',
    //marginLeft: ''
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    fontFamily: "Poppins",
    textAlign: "center",
    color: "black"
  },
  medfield: {
    fontSize: 16,
    fontWeight: "600",
    //textAlign: 'center',
    color: "#009CC6",
    marginBottom: 10,
    marginLeft: 24,
    marginRight: 24
  },

  button: {
    alignItems: "center",
    backgroundColor: "#00BCC6",
    padding: 6,
    borderRadius: 10,
    margin: 14
    //textAlign: 'center',
    //color: '#34495e',
  },
  EditButton: {
    alignItems: "center",
    backgroundColor: "#009CC6",
    padding: 6,
    borderRadius: 10,
    margin: 14 //display : state.reacted
  },
  RenewButton: {
    alignItems: "center",
    backgroundColor: "#50BB75",
    padding: 6,
    borderRadius: 10
    //display : state.reacted
  }
});
