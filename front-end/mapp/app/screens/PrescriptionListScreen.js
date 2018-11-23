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
} from "react-native";
import { Card, CardItem, Body } from "native-base";
import settings from "../config/settings";
import ActionButton from "react-native-action-button";
import genAlert from "../components/generalComponents/genAlert"
import PushNotification from "react-native-push-notification";
import {setupPushNotification} from "../lib/setupPushNotification"
import {getNewDate} from "../lib/getNewDate"
import { NavigationActions } from 'react-navigation';

class PrescriptionListScreen extends Component {
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
      <TouchableWithoutFeedback
        onPress={() => this.prescriptionOnPress(prescription.id)}
        key={prescription.id}
      >
        <Card>
          <CardItem header bordered>
            <Text style={styles.text}>{prescription.medication}</Text>
          </CardItem>
          <CardItem bordered>
            <Body>
              <View>
                <Text>Doctor: {doctor.firstName + " " + doctor.lastName}</Text>
                <Text>Dosage Unit: {prescription.dosageUnit}</Text>
                <Text>Amount Initial: {prescription.amountInitial}</Text>
              </View>
            </Body>
          </CardItem>
        </Card>
      </TouchableWithoutFeedback>
    );
  };

  render() {
    const prescriptions = this.props.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    return (
      <View style={{ flex: 1 }}>
        <ScrollView style={{ flex: 1 }}>
          {prescriptionIDs.map(id =>
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

const styles = StyleSheet.create({
  text: {
    color: settings.THEME_COLOR,
    fontSize: 20
  }
});
export default PrescriptionListScreen;
AppRegistry.registerComponent(
  "PrescriptionListScreen",
  () => PrescriptionListScreen
);
