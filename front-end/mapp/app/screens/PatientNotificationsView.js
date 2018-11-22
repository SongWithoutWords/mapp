import * as React from 'react';
import { Text, View, StyleSheet, TextInput, Image, ScrollView, Button, TouchableOpacity, AppRegistry } from 'react-native';
import { Card, CheckBox } from 'react-native-elements'; // 0.19.1
import checkRequestErrors from "../lib/errors";
import genAlert from "../components/generalComponents/genAlert";

import settings from "../config/settings";
// or any pure javascript modules available in npm

export default class PatientNotificationsView extends React.Component  {

  takeMed = prescription => {
    /*{ doseTakenPrescription = PrescriptionKey 1
         , doseTakenTime = timeFromString "2019-01-01 09:07"
         , doseTakenAmount = 0.5
       }*/

   return fetch(settings.REMOTE_SERVER_URL + settings.DOSES_RES, {
     method: "POST",
     headers: {
       Accept: "application/json",
       "Content-Type": "application/json"
     },
     body: JSON.stringify({
       doseTakenPrescription: prescription.id,
       doseTakenTime: new Date(),
       doseTakenAmount : 1
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
       //genAlert(error.name, error.message);
       alert(this);
     });
  }
  mapNotificationToCard = (prescription, mins) => (

      <Card flexDirection= 'row'>

          <View style = {[{width: 80, height: 80, borderColor : 'black', alignItems: 'center', justifyContent: 'center', padding: 10}, styles.withBottomBorder]}>
            <Image style = {{ width: 45, height: 45 }} source={require('./002-pill.png')} />
          </View>
          <View style = {{width: '75%', height: 100, padding: 5}}>
            <Text style={styles.fieldValue}>
              You must take your <Text style={styles.medfield, {color: '#C60000'}}>{prescription.medName}</Text> prescription <Text style={styles.medfield, {color: '#C60000'}}>now</Text>
            </Text>
            <Text style={styles.notificationDate}>{mins} minutes ago</Text>
            <View style={{
                      alignItems : 'center',
                      justifyContent : 'center',
                      flexDirection: 'row',
                      marginLeft: 0
                    }}>
              <View style={{width: '40%'}}>
              <TouchableOpacity style={styles.RenewButton} onPress = {this.takeMed.bind(this, prescription)}>
                <Text style = {styles.buttonText}>Take Now</Text>
              </TouchableOpacity>
              </View>
              <View style={{width: '40%'}}>
              <TouchableOpacity style={styles.EditButton}>
                <Text style = {styles.buttonText}>Snooze</Text>
              </TouchableOpacity>
              </View>
            </View>
          </View>
        </Card>
      //</View>
    );

  render() {
    const prescriptions = {byId : {1 : {  medName: "Cefixime",
      medDoseUnit: "Gram",
      medInitialAmount: 10,
      medRemainingAmoount: 6,
      startDate: new Date() ,
      endDate: new Date(),
      medFreq: 480},
      2 : {  medName: "Amoxicillin",
        medDoseUnit: "Gram",
        medInitialAmount: 10,
        medRemainingAmoount: 6,
        startDate: new Date().setMinutes(35) ,
        endDate: new Date(),
        medFreq: 480}},
    allIds:[1, 2]};
    const requestIDs = [1, 2];
    const one_min= 1000*60 ;
    const notificationInterval = 10;
    return (
      <ScrollView>
      {requestIDs.map(id => {
        alert((new Date() - prescriptions.byId[id].startDate)/one_min);
        if(((new Date() - prescriptions.byId[id].startDate)/one_min %  prescriptions.byId[id].medFreq) <= notificationInterval){
          let mins = (new Date() - prescriptions.byId[id].startDate)/one_min %  prescriptions.byId[id].medFreq;
          return this.mapNotificationToCard(prescriptions.byId[id], Math.round(mins));
        }
      })}
      </ScrollView>
    );
  }
}

/*<View style={{
          alignItems : 'center',
          justifyContent : 'center',
          flexDirection: 'row',
          marginLeft: 0
        }}>
        <View style={{width: '40%'}}>
        <TouchableOpacity style={styles.RenewButton}>
          <Text style = {styles.buttonText}>Take Now</Text>
        </TouchableOpacity>
        </View>
        <View style={{width: '40%'}}>
        <TouchableOpacity style={styles.EditButton}>
          <Text style = {styles.buttonText}>Snooze</Text>
        </TouchableOpacity>
        </View>
        </View>*/


const styles = StyleSheet.create({
  container: {
    //alignItems: 'center',
    //justifyContent: 'center',
    padding: 10,
    backgroundColor: '#ecf0f1',
    flex: 1
  },
  paragraph: {
    margin: 24,
    fontSize: 18,
    fontWeight: 'bold',
    textAlign: 'center',
  },
  notificationBox: {
    flex: 1,
    flexDirection: 'row',
    borderColor : "#009CC6",
    //marginTop: 40,
    padding : 10,
    backgroundColor: 'white',
    height: 110
  },
  notificationDate:{
    fontSize: 16,
    fontWeight: "200",
    fontFamily: 'Poppins',
    //textAlign: 'center'
    color:'#5A5A5A',
    marginTop : 3,
    marginLeft: 5
  },
  withBottomBorder: {
    borderColor: '#009CC6',
    borderWidth: 1,
    borderRadius: 50,
    backgroundColor: 'rgba(80, 187, 117, 0.2)'
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    fontFamily: 'Poppins',
    //textAlign: 'center',
    color: 'black',
    marginLeft: 5
  },
  medfield: {
    fontSize: 16,
    fontWeight: '600',
    fontFamily: 'Circular',
    //textAlign: 'center',
    color: '#009CC6',
    //marginTop: 40,
    marginLeft: 24,
    //marginRight: 24,
  },
  button: {
    margin: 14,
    fontSize: 15,
    fontFamily:'Circular',
    fontWeight: '400',
    width:'40%',
    color: 'white'
    //textAlign: 'center',
    //color: '#34495e',
  },
  buttonText: {
    color : 'white',
    fontFamily: 'Circular',
    fontWeight:'500',
    fontSize: 15
  },
  EditButton: { alignItems: 'center',
    backgroundColor: '#C60000',padding: 6, borderRadius:10 ,margin: 10,
  },
  RenewButton: { alignItems: 'center',
    backgroundColor: '#50BB75',padding: 6, borderRadius:10
  },

});
AppRegistry.registerComponent('PatientNotificationsView', () => PatientNotificationsView);
