import * as React from "react";
import {
  Text,
  View,
  StyleSheet,
  TextInput,
  Image,
  ScrollView,
  Button,
  TouchableOpacity,
  AppRegistry
} from "react-native";
import { Card, CheckBox } from "react-native-elements"; // 0.19.1
import checkRequestErrors from "../lib/errors";
import genAlert from "../components/generalComponents/genAlert";
import postData from "../lib/postData";

import settings from "../config/settings";
// or any pure javascript modules available in npm

export default class PatientNotificationsView extends React.Component {
  takeMed = prescription => {
    /*{ doseTakenPrescription = PrescriptionKey 1
         , doseTakenTime = timeFromString "2019-01-01 09:07"
         , doseTakenAmount = 0.5
       }*/
    const url = settings.REMOTE_SERVER_URL + settings.DOSES_RES;
    const data = {
      prescription: prescription.id,
      time: new Date(Date.now()),
      amount: 3
    };
    const { email, password } = this.props.screenProps.user;
    return postData(url, data, email, password)
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
        genAlert(error.name, error.message, error.errors);
        console.log(
          JSON.stringify({
            prescription: prescription.id,
            time: new Date(Date.now()),
            amount: 1
          })
        );
      });
  };
  mapNotificationToCard = (prescription, mins) => (
    <Card flexDirection= 'row' key={prescription.id}>
          <View style = {[{width: '25%',  aspectRatio: 1, alignItems: 'center', justifyContent: 'center', padding: 10}, styles.withBottomBorder]}>
            <Image style = {{  width: '80%', height: '80%' }} source={require('./002-pill.png')} />
          </View>
          <View style = {{width: '75%', height: 100, padding: 5}}>
            <Text style={styles.fieldValue}>
              You must take your <Text style={styles.medfield, {color: '#C60000'}}>{prescription.medication}</Text> prescription <Text style={styles.medfield, {color: '#C60000'}}>now</Text>
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
    );

  render() {
    const prescriptions = this.props.screenProps.prescriptions;
    const requestIDs = this.props.screenProps.user.myPrescriptions;
    console.log(requestIDs);
    const one_min = 1000 * 60;
    const notificationInterval = 10;
    return (
      <ScrollView>
        {requestIDs.map(id => {
          console.log(prescriptions.byId[id].dosesTaken);
          console.log(
            prescriptions.byId[id].dosesTaken[
              prescriptions.byId[id].dosesTaken.length - 1
            ]
          );

          let startDate = new Date(
            prescriptions.byId[id].dosageSchedule[0].firstDose
          );
          //Has the prescription started yet?
          if((new Date() - startDate) <= 0) return;
          //Right now freq seems to be 0, change it to 1 for testing
          let freq = prescriptions.byId[id].dosageSchedule[0].minutesBetweenDoses;
          let lastDose = undefined;
          if (prescriptions.byId[id].dosesTaken.length != 0)
            lastDose =
              prescriptions.byId[id].dosesTaken[
                prescriptions.byId[id].dosesTaken.length - 1
              ];

          let numOfDose = Math.floor(
            (new Date() - startDate) / (one_min * freq)
          );
          let numOfLastDose = -1;
          if (lastDose !== undefined)
            numOfLastDose = Math.floor(
              (new Date(lastDose.time) - startDate) / (one_min * freq)
            );

          if (numOfLastDose != numOfDose) {
            let mins = (new Date() - startDate) / one_min - numOfDose * freq;
            console.log('baaadddh');
            console.log(startDate);
            console.log('freq: ' + id + '' + prescriptions.byId[id].dosageSchedule[0]);
            console.log('nomofdose: ' + numOfDose);
            return this.mapNotificationToCard(
              prescriptions.byId[id],
              Math.round(mins)
            );
          }
        })}
      </ScrollView>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    padding: 10,
    backgroundColor: "#ecf0f1",
    flex: 1
  },
  paragraph: {
    margin: 24,
    fontSize: 18,
    fontWeight: "bold",
    textAlign: "center"
  },
  notificationBox: {
    flex: 1,
    flexDirection: "row",
    borderColor: "#009CC6",
    padding: 10,
    backgroundColor: "white",
    height: 110
  },
  notificationDate: {
    fontSize: 16,
    fontWeight: "200",
    fontFamily: "Poppins",
    color: "#5A5A5A",
    marginTop: 3,
    marginLeft: 5
  },
  withBottomBorder: {
    borderColor: "#009CC6",
    borderWidth: 1,
    borderRadius: 50,
    backgroundColor: "rgba(80, 187, 117, 0.2)"
  },
  fieldValue: {
    fontSize: 14,
    fontWeight: "200",
    fontFamily: 'Poppins',
    color: 'black',
    marginLeft: 5,
    fontFamily: 'Poppins-Regular'
  },
  medfield: {
    fontSize: 16,
    fontWeight: '600',
    color: '#009CC6',
    marginLeft: 24,
  },
  button: {
    margin: 14,
    fontSize: 15,
    fontWeight: "400",
    width: "40%",
    color: "white"
  },
  buttonText: {
    color : 'white',
    fontSize: 15,
    fontFamily: 'lineto-circular-pro-medium'
  },
  EditButton: { alignItems: 'center',
    backgroundColor: '#C60000',padding: 6, borderRadius:10 , marginTop: 5, marginLeft: 10
  },
  RenewButton: { alignItems: 'center',
    backgroundColor: '#50BB75',padding: 6, borderRadius:10 , marginTop: 5
  },
  RenewButton: {
    alignItems: "center",
    backgroundColor: "#50BB75",
    padding: 6,
    borderRadius: 10
  }
});
AppRegistry.registerComponent(
  "PatientNotificationsView",
  () => PatientNotificationsView
);
