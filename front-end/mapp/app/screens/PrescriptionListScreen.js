import React, { Component } from "react";
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
import PushNotification from "react-native-push-notification";
import {setupPushNotification} from "../lib/setupPushNotification"
import {getNewDate} from "../lib/getNewDate"
import { NavigationActions } from 'react-navigation';

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
    this.pushNotification = setupPushNotification(this.handleNotificationOpen)
  }

  handleNotificationOpen = (notification) => {
    this.props.navigation.navigate("Inbox");
    if(Platform.OS === 'ios')
      notification.finish(PushNotificationIOS.FetchResult.NoData);
  }

  fabOnPress = () => {
    genAlert("Adding a new prescription by patients", "Has not been implemented :(");
  };

  mapPrescriptionToCard = prescription => {
    //console.log(prescription.amountRemaining);
    let amountRemaining = prescription.amountInitial - prescription.dosesTaken.length*prescription.dosageSchedule[0].dosage
    const doctor = this.props.screenProps.doctors.byId[prescription.doctor];
    const numberLeft = (prescription.amountInitial / prescription.dosageSchedule[0].dosage) - Object.keys(prescription.dosesTaken).length;
    const newTime = getNewDate(prescription.dosageSchedule[0].firstDose, prescription.dosageSchedule[0].minutesBetweenDoses);
    console.log(doctor + " " + numberLeft + " " + newTime);
    PushNotification.cancelLocalNotifications({id: prescription.id + ""});
    if(numberLeft > 0){
      PushNotification.localNotificationSchedule({
        /* Android Only Properties */
        id: prescription.id + "", 
        ticker: "Notification to take prescription",
        autoCancel: true, 
        largeIcon: "ic_launcher", 
        smallIcon: "ic_notification", 
        bigText: "It is time to take " + prescription.medication + " press to goto inbox screen", 
        subText: "Time to take your medicine!",
        color: "purple", 
        vibrate: true, 
        vibration: 300, // vibration length in milliseconds, ignored if vibrate=false, default: 1000
        tag: 'mapp_prescription_notification', 
        group: "prescription_notification", 
        ongoing: false, // (optional) set whether this is an "ongoing" notification
        priority: "high", // (optional) set notification priority, default: high
        visibility: "public", // (optional) set notification visibility, default: private
        importance: "high", // (optional) set notification importance, default: high
        /* ios only */
        userInfo: {id : prescription.id + ""},
        /* iOS and Android properties */
        title: "Time to take your medicine!", 
        message: "It is time to take " + prescription.medication + " press to goto inbox screen", 
        playSound: true, 
        soundName: 'default', 
        number: numberLeft, // (optional) Valid 32 bit integer specified as string. default: none (Cannot be zero)
        repeatType: 'time', // every repeatTime ms run the notification again 
        repeatTime: prescription.dosageSchedule[0].minutesBetweenDoses * 60000,
        date: newTime
      });
    }

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
               Every 8 hours
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Location: <Text style={styles.fieldValue}>
               Kitchen - Under the sink - in the white box
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
            <TouchableOpacity style={styles.EditButton}>
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
        <ScrollView style={{ flex: 1 }}>
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
    // fontFamily: 'Poppins',
    textAlign: 'center',
    color: 'black',
  },
  medfield: {
    fontSize: 16,
    fontWeight: '600',
    // fontFamily: 'Circular',
    //textAlign: 'center',
    color: '#009CC6',
    marginTop: 10,
    marginBottom: 20,
    marginLeft: 24,
    marginRight: 24,
  },
  remainingPills: {
    textAlign: 'center',
    fontSize: 20,
    // fontFamily:'Circular',
    fontWeight: '400',
    color: 'black',
  },
  button: {
    margin: 24,
    fontSize: 22,
    // fontFamily:'Circular',
    fontWeight: '600',
    width:'30%',
    color: 'white'
    //textAlign: 'center',
    //color: '#34495e',
  },
  buttonText: {
    color : 'white',
    // fontFamily: 'Circular',
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
