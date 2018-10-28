import * as React from "react";
import checkRequestErrors from "../lib/errors";
import settings from "../config/settings";
import { Text, View, StyleSheet, AppRegistry } from "react-native";
import { Card } from "react-native-elements"; //0.19.1
import { TouchableOpacity } from "react-native";
import genAlert from "../components/generalComponents/genAlert";

export default class InboxScreen extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      showButtons: true,
      text: "",
      doctorID: -1,
      patientID: -1,
      pendingRequests: []
    };

    this.acceptPatientRequest = this.acceptPatientRequest.bind(this);
  }

  componentDidMount() {
    this.fetchDoctorData();
    this.setState({
      text: "you have new requests"
    });
  }

  declinePatientRequest = () => {
    this.setState({ text: "You decline", showButtons: false });
  };

  acceptPatientRequest = requestID => {
    return fetch(settings.REMOTE_SERVER_URL + settings.RELAITON_RES, {
      method: "POST",
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        patient: requestID,
        doctor: this.state.doctorID
      })
    })
      .then(checkRequestErrors)
      .then(response => response.json())
      .then(responseJson => {
        this.setState(
          {
            response: responseJson
          },
          function() {}
        );
      })
      .catch(error => {
        genAlert(error.name, error.message);
      });
  };

  fetchDoctorData() {
    return fetch(
      settings.REMOTE_SERVER_URL + settings.DOCTOR_RES + this.state.doctorID
    )
      .then(checkRequestErrors)
      .then(response => response.json())
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
      <View style={styles.container}>
        {this.state.pendingRequests.map(request => (
          <Card>
            <Text style={styles.medfield}>
              <Text style={styles.fieldValue}>{this.state.text}</Text>{" "}
              {request.firstName + request.lastName}
            </Text>
            <View
              style={{
                alignItems: "center",
                justifyContent: "center",
                flexDirection: "row"
              }}
            >
              {this.state.showButtons && (
                <View style={{ width: "40%" }}>
                  <TouchableOpacity
                    onPress={this.acceptPatientRequest(
                      this.state.pendingRequests[i].id
                    )}
                    style={styles.RenewButton}
                  >
                    <Text
                      style={{
                        color: "white",
                        fontWeight: "500",
                        fontSize: 16
                      }}
                    >
                      Accept
                    </Text>
                  </TouchableOpacity>
                </View>
              )}
              {this.state.showButtons && (
                <View style={{ width: "40%" }}>
                  <TouchableOpacity
                    onPress={this.declinePatientRequest}
                    style={styles.EditButton}
                  >
                    <Text
                      style={{
                        color: "white",
                        fontWeight: "500",
                        fontSize: 16
                      }}
                    >
                      Decline
                    </Text>
                  </TouchableOpacity>
                </View>
              )}
            </View>
          </Card>
        ))}
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
