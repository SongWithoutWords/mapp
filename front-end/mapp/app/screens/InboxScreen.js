import * as React from 'react';
import { Text, View, StyleSheet, ScrollView, TextInput, AppRegistry} from 'react-native';
//import { Constants } from 'expo';

// You can import from local files
//import AssetExample from './components/AssetExample';

// or any pure javascript modules available in npm
import { Card } from 'react-native-elements'; //0.19.1
import { Button, TouchableOpacity, ProgressBar, Dimensions } from 'react-native';
//import ProgressBarAnimated from 'react-native-progress-bar-animated';
export default class InboxScreen extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      showButtons: true,
      text: 'You have a new request from',
      doctorID: 1,
      pendingRequests: [{id: 3}]
    }

    this.acceptPatientRequest = this.acceptPatientRequest.bind(this);
  }
  componentDidMount(){
    this.fetchDoctorData();
  }
  declinePatientRequest = () => {
    this.setState({text: 'You decline', showButtons: false})
  }
  acceptPatientRequest = (i) => {
    return fetch('http://www.agis-mapp.xyz/relations' , {
      method: 'POST',
      headers: {
        Accept: 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        patient: i,
        doctor: 1,
      }),
      }).then((response) => response.json())
      .then((responseJson) => {

        this.setState({
          isLoading: false,
          dataSource: responseJson,
          //showButtons: false,
          //text: 'You have accepted a request'
        }, function(){
           alert(JSON.stringify(responseJson));
        });

      }).catch((error) => {
          console.error(error);
    });
  }
  fetchDoctorData(){
    return fetch('http://www.agis-mapp.xyz/doctors/' + this.state.doctorID)
    .then((response) => response.json())
    .then((responseJson) => {

      this.setState({
        doctorID: responseJson.id,
        pendingRequests: responseJson.pendingRequests,
      });
      console.log(this.state.pendingRequests);
    })
    .catch((error) => {
      console.error(error);
    });
  }
  render() {
    return (

      <View style={styles.container}>
      {this.state.pendingRequests.map((request, i) => (
        <Card>
        <Text style={styles.medfield}>
          <Text style={styles.fieldValue}>
            {this.state.text}
          </Text> {request.firstName + request.lastName}
        </Text>
        <View style={{
          alignItems : 'center',
          justifyContent : 'center',
          flexDirection: 'row',
          marginLeft: ''
          //height: 100,
          //padding: 20,
        }}>

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
      ))}
      <TouchableOpacity onPress={this.fetchDoctorData.bind(this)} style={styles.button}>
          <Text style = {{color : 'white', fontFamily: 'Circular', fontWeight:'500', fontSize: 16}}>Refresh</Text>
      </TouchableOpacity>
      </View>

    );
  }
}

AppRegistry.registerComponent('InboxScreen', () => InboxScreen);

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    //justifyContent: 'center',
    //paddingTop: Constants.statusBarHeight,
    backgroundColor: '#ecf0f1',
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    fontFamily: 'Poppins',
    textAlign: 'center',
    color: 'black',
  },
  medfield: {
    fontSize: 16,
    fontWeight: '600',
    fontFamily: 'Circular',
    //textAlign: 'center',
    color: '#009CC6',
    marginBottom: 10,
    marginLeft: 24,
    marginRight: 24,
  },

  button: {
    alignItems: 'center',
    backgroundColor: '#00BCC6',padding: 6, borderRadius:10 ,margin: 14,
    //textAlign: 'center',
    //color: '#34495e',
  },
  EditButton: { alignItems: 'center',
    backgroundColor: '#009CC6',padding: 6, borderRadius:10 ,margin: 14,              //display : state.reacted
  },
  RenewButton: { alignItems: 'center',
    backgroundColor: '#50BB75',padding: 6, borderRadius:10,
    //display : state.reacted
  },

});
