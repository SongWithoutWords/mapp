import * as React from 'react';
import { Text, View, StyleSheet,ScrollView,
    TextInput, } from 'react-native';
import { Constants } from 'expo';

// You can import from local files
//import AssetExample from './components/AssetExample';

// or any pure javascript modules available in npm
import { Card } from 'react-native-elements'; // 0.19.1
import { Button, TouchableOpacity, ProgressBar, Dimensions } from 'react-native';
//import ProgressBarAnimated from 'react-native-progress-bar-animated';
export default class App extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      showButtons: true,
      text: 'You have a new request from'
    }

    this.acceptPatientRequest = this.acceptPatientRequest.bind(this);
  }
  declinePatientRequest = () => {
    this.setState({text: 'You declined', showButtons: false})
  }
  acceptPatientRequest() {
    return fetch('http://www.agis-mapp.xyz/relations' , {
      method: 'POST',
      headers: {
        Accept: 'application/json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        patient: 1,
        doctor: 1,
      }),
      }).then((response) => response.json())
      .then((responseJson) => {

        this.setState({
          isLoading: false,
          dataSource: responseJson,
          showButtons: false,
          text: 'You have accepted a request'
        }, function(){
           alert(JSON.stringify(responseJson));
        });

      }).catch((error) => {
          console.error(error);
    });
  }
  render() {
    return (

      <View style={styles.container}>

        <Card>
        <Text style={styles.medfield}>
          <Text style={styles.fieldValue}>
            {this.state.text}
          </Text> Sina Saleh
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
        <TouchableOpacity onPress={this.acceptPatientRequest} style={styles.RenewButton}>
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
        <Card>
        <Text style={styles.medfield}>
          <Text style={styles.fieldValue}>
            {this.state.text}
          </Text> Sina Saleh
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
        <TouchableOpacity onPress={this.acceptPatientRequest} style={styles.RenewButton}>
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
      </View>

    );
  }
}
const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    //justifyContent: 'center',
    paddingTop: Constants.statusBarHeight,
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
    margin: 24,
    fontSize: 22,
    fontFamily:'Circular',
    fontWeight: '600',
    width:'30%',
    color: 'white',
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
