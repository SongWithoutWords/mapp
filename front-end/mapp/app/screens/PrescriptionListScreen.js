import React, { Component } from "react";
import _ from "lodash"
import {
  Text,
  View,
  TouchableWithoutFeedback,
  AppRegistry,
  ScrollView,
  StyleSheet,
  Platform,
  PushNotificationIOS,
  Dimensions,
  TouchableOpacity
} from "react-native";
import { Card, CardItem, Body } from "native-base";
import settings from "../config/settings";
import ActionButton from "react-native-action-button";
import genAlert from "../components/generalComponents/genAlert"
import {setupPushNotification} from "../lib/setupPushNotification"
import {scheduleNotifications} from "../lib/scheduleNotifications"

import ProgressBarAnimated from 'react-native-progress-bar-animated';
class PrescriptionListScreen extends Component {

  // polling on server
  componentDidMount() {
    const { email, password } = this.props.screenProps.user;
    const form = { email, password };
    const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
    this.timer = setInterval(() => {
      this.props.screenProps.onSignIn(url, form);
    }, 2000);
  }

  componentWillUnmount() {
    clearInterval(this.timer);
    this.timer = null; // here...
  }


  prescriptionOnPress = id => {
    this.props.navigation.navigate("PrescriptionInfo", {
      prescription: this.props.screenProps.prescriptions.byId[id]
    });
  };

  componentWillMount(){
    this.pushNotification = setupPushNotification(this.handleNotificationOpen);
    const prescriptions = this.props.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    prescriptionIDs.map(id =>scheduleNotifications(prescriptions.byId[id]));
  }

  componentDidUpdate(prevProps){
    const prescriptions = this.props.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    if (_.isEqual(prevProps.screenProps.prescriptions, prescriptions))
      console.log("they are equal!");
    else
      prescriptionIDs.map(id =>scheduleNotifications(prescriptions.byId[id]));
  }

  handleNotificationOpen = (notification) => {
    this.props.navigation.navigate("Inbox");
    if(Platform.OS === 'ios')
      notification.finish(PushNotificationIOS.FetchResult.NoData);
  }

  fabOnPress = () => {
    genAlert("Adding a new prescription by patients", "Has not been implemented :(");
  };

  onEditPress = prescription => {
    //console.log(id);
    if(prescription.dosesTaken.length == 0){
      this.props.navigation.navigate("EditPrescription", {
        prescription: prescription
      });
    }
    else{
      genAlert("You have already started this prescription.");
    }
  };

  convertMinsToFreqString = mins => {
    switch (mins) {
      case (24 * 60):
        return "Every day";
      case (1):
        return "Every minute";
      case (7 * 24 * 60):
        return "Every week";
      default:
        return "Every " + mins + " minutes";
    }
  };

  mapPrescriptionToCard = prescription => {
    let amountRemaining = prescription.amountInitial - (prescription.dosesTaken.length*prescription.dosageSchedule[0].dosage);
    const doctor = this.props.screenProps.doctors.byId[prescription.doctor];
    return (
        <Card style={styles.container} key={prescription.id}>
          <Text style={styles.medfield}>
            Medication: <Text style={styles.fieldValue}>
               {prescription.medication}
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Dosage Unit: <Text style={styles.fieldValue}>
               {prescription.dosageUnit}
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Physician: <Text style={styles.fieldValue}>
              {doctor.firstName + " " + doctor.lastName}
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Frequency: <Text style={styles.fieldValue}>
              {this.convertMinsToFreqString(prescription.dosageSchedule[0].minutesBetweenDoses)} 
            </Text>
          </Text>
          <View style={{
            alignItems : 'center',
          }}>
              <ProgressBarAnimated
                width={barWidth}
                value={Math.round(amountRemaining / prescription.amountInitial * 100)}
                height = {20}
                backgroundColor="#6CC644"
                barAnimationDuration = {0}
              />
              <View style={styles.buttonContainer}>
                <View style={styles.buttonInner}>
                  <Text style = {styles.remainingPills}>{amountRemaining}/{prescription.amountInitial} {prescription.dosageUnit}s</Text>
                </View>
              </View>
            </View>
            <View style={{
              alignItems : 'center',
              justifyContent : 'center',
              flexDirection: 'row',
              marginLeft: 0
            }}>
            <View style={{width: '40%'}}>
            <TouchableOpacity style={styles.RenewButton}>
              <Text style = {styles.buttonText}>Renew</Text>
            </TouchableOpacity>
            </View>
            <View style={{width: '40%'}}>
            <TouchableOpacity style={styles.EditButton} onPress = {this.onEditPress.bind(this, prescription)}>
              <Text style = {styles.buttonText}>Edit</Text>
            </TouchableOpacity>
            </View>
          </View>
        </Card>
    );
  };

  render() {
    const prescriptions = this.props.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    return (
      <View style={{ flex: 1 }}>
        <ScrollView style={{ flex: 1 }, styles.container}>
          {
            prescriptionIDs.map(id =>
            this.mapPrescriptionToCard(prescriptions.byId[id])
          )}
        </ScrollView>
        <ActionButton
          buttonColor={settings.THEME_COLOR}
          onPress={this.fabOnPress}
        />
      </View>
    );
  }
}

const barWidth = Dimensions.get('screen').width*0.7;

const styles = StyleSheet.create({
  text: {
    color: settings.THEME_COLOR,
    fontSize: 20
  },
  container: {
    flex: 1,
    padding: 10,
    backgroundColor: 'white',
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    textAlign: 'center',
    color: 'black',
  },
  medfield: {
    fontSize: 16,
    fontWeight: '600',
    color: '#009CC6',
    marginTop: 10,
    marginBottom: 20,
    marginLeft: 24,
    marginRight: 24,
  },
  remainingPills: {
    textAlign: 'center',
    fontSize: 20,
    fontWeight: '400',
    color: 'black',
  },
  button: {
    margin: 24,
    fontSize: 22,
    fontWeight: '600',
    width:'30%',
    color: 'white'
  },
  buttonText: {
    color : 'white',
    fontWeight:'500',
    fontSize: 16
  },
  EditButton: { alignItems: 'center',
    backgroundColor: '#009CC6',padding: 6, borderRadius:10 ,margin: 14,
  },
  RenewButton: { alignItems: 'center',
    backgroundColor: '#50BB75',padding: 6, borderRadius:10
  },
});
export default PrescriptionListScreen;
AppRegistry.registerComponent(
  "PrescriptionListScreen",
  () => PrescriptionListScreen
);
