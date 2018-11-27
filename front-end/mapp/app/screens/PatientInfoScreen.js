import React, { Component } from "react";
import {
  Text,
  View,
  Button,
  TouchableWithoutFeedback,
  AppRegistry,
  ScrollView,
  StyleSheet,
  Platform,
  PushNotificationIOS,
  Dimensions,
  TouchableOpacity
} from "react-native";
import { Card } from "react-native-elements";
import genAlert from "../components/generalComponents/genAlert";
import { setupPushNotification } from "../lib/setupPushNotification";
import { scheduleNotifications } from "../lib/scheduleNotifications";
import { convertMinsToFreqString } from "../lib/frequencyMinsConversion";
import ProgressBarAnimated from "react-native-progress-bar-animated";
import { getLocalDateTimeString } from "../lib/dateTime";
import fetchAuth from "../lib/fetchAuth";
import settings from "../config/settings";
import createPrescription from "../lib/createPrescription";
import deletePrescription from "../lib/deletePrescription";

class PatientInfoScreen extends Component {
  deletePatientRelation = patient => {

    const url =
      settings.REMOTE_SERVER_URL +
      settings.RELAITON_RES +
      "/" +
      patient.relationId;
    console.log('bekhoda khari');
    console.log(patient);
    const { email, password } = this.props.screenProps.user;
    const method = "DELETE";
    return fetchAuth({url, method, email, password})
      .then(response => {
        genAlert("Patient deleted!");
        this.props.navigation.goBack();
      })
      .catch(error => {
        genAlert("Failed to Patient the relationship", error.message);
    });
  }
  onRenewPress = prescription => {
    let amountRemaining =
      prescription.amountInitial -
      prescription.dosesTaken.length * prescription.dosageSchedule[0].dosage;
    if (amountRemaining <= 0) {
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
        password: this.props.screenProps.user.password
      });
      deletePrescription({ prescriptionID: prescription.id , navigation: null , email: this.props.screenProps.user.email, password: this.props.screenProps.user.password});
    }
  };
  onEditPress = prescription => {
    console.log('baba goh nakhor');
    console.log(prescription);
    console.log(this.props.screenProps.user.id);
    if (prescription.dosesTaken.length !== 0) {
      genAlert("Patient has already started this prescription.");
    } else if (prescription.doctor === null) {
      genAlert("You can edit only the prescriptions you created.");
    } else {
      this.props.navigation.navigate("DoctorEditPrescription", {
        prescription: prescription,
        patient: this.props.navigation.getParam("patient", {}),
        user: this.props.navigation.getParam("user", {})
      });
    }
  };
  mapPrescriptionToCard = (prescription, user_id) => {
    let amountRemaining =
      prescription.amountInitial -
      prescription.dosesTaken.length * prescription.dosageSchedule[0].dosage;
    var doctor;
    var doctorField;
    console.log('nnananannaddan');
    if(amountRemaining < 0) return
    if(prescription.doctor != user_id) return
    const firstDoseString = getLocalDateTimeString(prescription.dosageSchedule[0].firstDose.toString());
    const frequency = convertMinsToFreqString(
      prescription.dosageSchedule[0].minutesBetweenDoses
    );
    const dosage = prescription.dosageSchedule[0].dosage;
    const val = Math.round((amountRemaining / prescription.amountInitial) * 100);
    return (
      <Card style={styles.container} key={prescription.id}>
        <Text style={styles.medfield}>
          Medication:{" "}
          <Text style={styles.fieldValue}>{prescription.medication}</Text>
        </Text>
        <Text style={styles.medfield}>
          Dosage:{" "}
          <Text style={styles.fieldValue}>{dosage} {prescription.dosageUnit}</Text>
        </Text>
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
            value={val}
            height={20}
            backgroundColor={'#' + Math.round(((0xC6 - 0x6C)/100*(100-val) + 0x6C)).toString(16)
            + Math.round(((0x00 - 0xC6)/100*(100-val) + 0xC6)).toString(16) + '00'}
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
            <TouchableOpacity style={styles.RenewButton} onPress={this.onRenewPress.bind(this, prescription)}>
              <Text style={styles.buttonText}>Renew</Text>
            </TouchableOpacity>
          </View>
          <View style={{ width: "40%" }}>
            <TouchableOpacity
              style={styles.EditButton} onPress={this.onEditPress.bind(this, prescription)}
            >
              <Text style={styles.buttonText}>Edit</Text>
            </TouchableOpacity>
          </View>
        </View>
      </Card>
    );
  };
  render() {
    const patient = this.props.navigation.getParam("patient", {});
    const user = this.props.navigation.getParam("user", {});

    const prescriptions = this.props.screenProps.patients.byId[patient.id].prescriptions;

    return (
      <View style={{ flex: 1 }}>
        <ScrollView style={styles.container}>
          <Card flexDirection= 'row'>
          <View style={{width: '50%', justifyContent:'center'}}>
          <Text style = {styles.medfield}>{
                        patient.firstName +
                        " " +
                        patient.lastName}</Text>
          </View>
          <View style={{width: '15%', justifyContent:'center'}}>
          <Text style = {styles.doctorName}>
                      ID: {patient.id}</Text>
          </View>
          <View style={{width: '35%', justifyContent:'center', alignItems: 'center', flex:1}}>
          <TouchableOpacity
                style={styles.submitButton}
                onPress={this.deletePatientRelation.bind(this, this.props.screenProps.patients.byId[patient.id])}
              >
                <Text style={styles.buttonText}> Delete</Text>
              </TouchableOpacity>
          </View>
          </Card>
          {prescriptions.map(prescription => this.mapPrescriptionToCard(prescription, user.id))
          }
        </ScrollView>
        <Button
          title="Create a new prescription"
          color="#50BB75"
          onPress={() =>
            this.props.navigation.navigate("DoctorMakePrescription", {
              patient: patient,
              user: user
            })
          }
        />
      </View>
    );
  }
}
/*<Text style={styles.medfield}>
  Patient Name: {patient.firstName} {patient.lastName}
</Text>
<Text style={styles.medfield}>
  Patient ID: {patient.id}
</Text>
<Text style={styles.medfield}>
  Date of Birth: {patient.dateOfBirth}
</Text>*/
const barWidth = Dimensions.get("screen").width * 0.7;

const styles = StyleSheet.create({
  container: {
    flex: 1,
    padding: 10,
    backgroundColor: "white"
  },
  fieldValue: {
    fontSize: 16,
    textAlign: "center",
    color: "black",
    fontFamily: "Poppins-Regular"
  },
  medfield: {
    fontSize: 16,
    color: "#009CC6",
    marginTop: 10,
    marginBottom: 20,
    marginLeft: 24,
    marginRight: 24,
    fontFamily: "Poppins-SemiBold"
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
  },
  submitButton: {
    backgroundColor: "#C60000",
    padding: 8,
    height: 35,
    borderRadius: 10,
    alignItems: "center",
    color: 'white',
  }
});

export default PatientInfoScreen;
AppRegistry.registerComponent("PatientInfoScreen", () => PatientInfoScreen);
