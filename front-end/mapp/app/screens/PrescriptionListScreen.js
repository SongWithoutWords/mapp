import React, { Component } from "react";
import _ from "lodash";
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
import { FREQUENCY, DOSAGE_UNIT } from "../config/constants";
import { Card, CardItem, Body } from "native-base";
import settings from "../config/settings";
import ActionButton from "react-native-action-button";
import genAlert from "../components/generalComponents/genAlert";
import { setupPushNotification } from "../lib/setupPushNotification";
import { scheduleNotifications } from "../lib/scheduleNotifications";
import { sendNotification } from "../lib/sendNotification";
import { convertMinsToFreqString } from "../lib/frequencyMinsConversion";
import createPrescription from "../lib/createPrescription";
import ProgressBarAnimated from "react-native-progress-bar-animated";
class PrescriptionListScreen extends Component {
  componentWillMount() {
    this.pushNotification = setupPushNotification(this.handleNotificationOpen);
    const prescriptions = this.props.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    prescriptionIDs.map(id => {
      scheduleNotifications(prescriptions.byId[id]);
      numberLeft =
        prescriptions.byId[id].amountInitial /
          prescriptions.byId[id].dosageSchedule[0].dosage -
        Object.keys(prescriptions.byId[id].dosesTaken).length;
      if (
        (numberLeft * prescriptions.byId[id].dosageSchedule[0].dosage) /
          prescriptions.byId[id].amountInitial <
        0.2
      )
        sendNotification(
          "You can renew prescription for " +
            prescriptions.byId[id].medication +
            " just go to the prescriptions page to do so",
          "A prescription for is running low!"
        );
    });
  }

  componentDidUpdate(prevProps) {
    const newPrescriptions = this.props.screenProps.prescriptions;
    const oldPrescriptions = prevProps.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    if (_.isEqual(newPrescriptions, oldPrescriptions))
      console.log("they are equal!");
    else {
      prescriptionIDs.map(id => {
        scheduleNotifications(newPrescriptions.byId[id]);
        newNumberLeft =
          newPrescriptions.byId[id].amountInitial /
            newPrescriptions.byId[id].dosageSchedule[0].dosage -
          Object.keys(newPrescriptions.byId[id].dosesTaken).length;
        oldNumberLeft =
          oldPrescriptions.byId[id].amountInitial /
            oldPrescriptions.byId[id].dosageSchedule[0].dosage -
          Object.keys(oldPrescriptions.byId[id].dosesTaken).length;
        if (
          newNumberLeft - oldNumberLeft < 0 &&
          (newNumberLeft * newPrescriptions.byId[id].dosageSchedule[0].dosage) /
            newPrescriptions.byId[id].amountInitial <
            0.2
        )
          sendNotification(
            "You can renew prescription for " +
              newPrescriptions.byId[id].medication +
              ". Just go to the prescriptions page to do so",
            "A prescription is running low!"
          );
      });
    }
  }

  handleNotificationOpen = notification => {
    this.props.navigation.navigate("Inbox");
    if (Platform.OS === "ios")
      notification.finish(PushNotificationIOS.FetchResult.NoData);
  };

  fabOnPress = () => {
    this.props.navigation.navigate("PatientMakePrescription", {
      user: this.props.screenProps.user
    });
  };

  onRenewPress = prescription => {
    let amountRemaining =
      prescription.amountInitial -
      prescription.dosesTaken.length * prescription.dosageSchedule[0].dosage;
    if (prescription.doctor !== null && amountRemaining <= 0) {
      createPrescription({
        medication: prescription.medication,
        dosage: prescription.dosageSchedule[0].dosage,
        dosageUnit: prescription.dosageUnit,
        frequency: FREQUENCY.EVERY_WEEK,
        minutesBetweenDoses: prescription.dosageSchedule[0].minutesBetweenDoses,
        amountInitial: prescription.amountInitial,
        startDateTime: prescription.dosageSchedule[0].firstDose,
        patientID: prescription.patient,
        doctorID: prescription.doctor, // TODO
        navigation: this.props.navigation,
        email: this.props.screenProps.user.email,
        password:this.props.screenProps.user.password
      });
    }
  };
  onEditPress = prescription => {
    if (prescription.dosesTaken.length !== 0) {
      genAlert("You have already started this prescription.");
    } else if (prescription.doctor !== null) {
      genAlert("You can edit only the prescriptions you created.");
    } else {
      this.props.navigation.navigate("PatientEditPrescription", {
        prescription: prescription,
        user: this.props.screenProps.user
      });
    }
  };

  mapPrescriptionToCard = prescription => {
    console.log(prescription);
    let amountRemaining =
      prescription.amountInitial -
      prescription.dosesTaken.length * prescription.dosageSchedule[0].dosage;
    var doctor;
    var doctorField;
    if (amountRemaining < 0) return;
    if (prescription.doctor !== null) {
      doctor = this.props.screenProps.doctors.byId[prescription.doctor];
      doctorField = (
        <Text style={styles.medfield}>
          Doctor:{" "}
          <Text style={styles.fieldValue}>
            {doctor.firstName + " " + doctor.lastName}
          </Text>
        </Text>
      );
    } else {
      doctorField = <View />;
      doctor = null;
    }

    const firstDoseString = prescription.dosageSchedule[0].firstDose
      .toString()
      .slice(0, 10);
    const frequency = convertMinsToFreqString(
      prescription.dosageSchedule[0].minutesBetweenDoses
    );
    const dosage = prescription.dosageSchedule[0].dosage;
    const val = Math.round(
      (amountRemaining / prescription.amountInitial) * 100
    );
    return (
      <Card style={styles.container} key={prescription.id}>
        <Text style={styles.medfield}>
          Medication:{" "}
          <Text style={styles.fieldValue}>{prescription.medication}</Text>
        </Text>
        <Text style={styles.medfield}>
          Dosage:{" "}
          <Text style={styles.fieldValue}>
            {dosage} {prescription.dosageUnit}
          </Text>
        </Text>
        {doctorField}
        <Text style={styles.medfield}>
          Frequency: <Text style={styles.fieldValue}>{frequency}</Text>
        </Text>
        <Text style={styles.medfield}>
          First Dose: <Text style={styles.fieldValue}>{firstDoseString}</Text>
        </Text>
        <View
          style={{
            alignItems: "center"
          }}
        >
          <ProgressBarAnimated
            width={barWidth}
            value={Math.round(
              (amountRemaining / prescription.amountInitial) * 100
            )}
            height={20}
            backgroundColor={
              "#" +
              Math.round(((0xc6 - 0x6c) / 100) * (100 - val) + 0x6c).toString(
                16
              ) +
              Math.round(((0x00 - 0xc6) / 100) * (100 - val) + 0xc6).toString(
                16
              ) +
              "00"
            }
            barAnimationDuration={0}
          />
          <View style={styles.buttonContainer}>
            <View style={styles.buttonInner}>
              <Text style={styles.remainingPills}>
                {amountRemaining}/{prescription.amountInitial}{" "}
                {prescription.dosageUnit}s
              </Text>
            </View>
          </View>
        </View>
        <View
          style={{
            alignItems: "center",
            justifyContent: "center",
            flexDirection: "row",
            marginLeft: 0
          }}
        >
          <View style={{ width: "40%" }}>
            <TouchableOpacity
              style={styles.RenewButton}
              onPress={this.onRenewPress.bind(this, prescription)}
            >
              <Text style={styles.buttonText}>Renew</Text>
            </TouchableOpacity>
          </View>
          <View style={{ width: "40%" }}>
            <TouchableOpacity
              style={styles.EditButton}
              onPress={this.onEditPress.bind(this, prescription)}
            >
              <Text style={styles.buttonText}>Edit</Text>
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
        <ScrollView style={styles.container}>
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

const barWidth = Dimensions.get("screen").width * 0.7;

const styles = StyleSheet.create({
  text: {
    color: settings.THEME_COLOR,
    fontSize: 20
  },
  container: {
    flex: 1,
    padding: 10,
    backgroundColor: "white"
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    textAlign: "center",
    color: "black"
  },
  medfield: {
    fontSize: 16,
    fontWeight: "600",
    color: "#009CC6",
    marginTop: 10,
    marginBottom: 20,
    marginLeft: 24,
    marginRight: 24
  },
  remainingPills: {
    textAlign: "center",
    fontSize: 20,
    fontWeight: "400",
    color: "black"
  },
  button: {
    margin: 24,
    fontSize: 22,
    fontWeight: "600",
    width: "30%",
    color: "white"
  },
  buttonText: {
    color: "white",
    fontWeight: "500",
    fontSize: 16
  },
  EditButton: {
    alignItems: "center",
    backgroundColor: "#009CC6",
    padding: 6,
    borderRadius: 10,
    margin: 14
  },
  RenewButton: {
    alignItems: "center",
    backgroundColor: "#50BB75",
    padding: 6,
    borderRadius: 10
  }
});
export default PrescriptionListScreen;
AppRegistry.registerComponent(
  "PrescriptionListScreen",
  () => PrescriptionListScreen
);
