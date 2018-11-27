import * as React from "react";
import {
  View,
  StyleSheet,
  TextInput,
  ScrollView,
  AppRegistry
} from "react-native";
// or any pure javascript modules available in npm
import { Card, CheckBox } from "react-native-elements"; // 0.19.1
import settings from "../config/settings";
import patchData from "../lib/patchData";
import genAlert from "../components/generalComponents/genAlert";
import DateTimePicker from "react-native-modal-datetime-picker";
import { Picker, Header, Icon, Button, Text } from "native-base";
import { FREQUENCY, DOSAGE_UNIT } from "../config/constants";
import { convertFrequencyToMins, convertMinsToFreqString } from "../lib/frequencyMinsConversion";
import validatePrescription from "../lib/validatePrescription";
import modifyPrescription from "../lib/modifyPrescription";
import deletePrescription from "../lib/deletePrescription";

export default class EditPrescriptionView extends React.Component {
  constructor(props) {
    super(props);
    const prescription = this.props.navigation.getParam("prescription", {});
    this.state = {
      isStartDateTimePickerVisible: false,
      medication: prescription.medication,
      dosage: prescription.dosageSchedule[0].dosage,
      dosageUnit: prescription.dosageUnit,
      frequency: convertMinsToFreqString(prescription.dosageSchedule[0].minutesBetweenDoses), // used to compute minutes between doses
      minutesBetweenDoses: prescription.dosageSchedule[0].minutesBetweenDoses,
      amountInitial: prescription.amountInitial,
      startDateTime: null
    };
  }

  // date time picker handlers
  _showStartDateTimePicker = () =>
    this.setState({ isStartDateTimePickerVisible: true });
  _hideStartDateTimePicker = () =>
    this.setState({ isStartDateTimePickerVisible: false });
  _handleStartDatePicked = date => {
    console.log("A start date has been picked: ", date);
    this.setState({ startDateTime: date });
    this._hideStartDateTimePicker();
  };

  deletePrescriptionOnPress = () => {
    const user = this.props.navigation.getParam("user", {});
    const email = user.email
    const password = user.password;
    const prescriptionID = this.props.navigation.getParam("prescription", {})
      .id;
    const navigation = this.props.navigation;
    deletePrescription({ prescriptionID, navigation, email, password});
  };

  editPrescriptionOnPress = () => {
    var localState;
    const {
      medication,
      dosage,
      dosageUnit,
      frequency,
      minutesBetweenDoses,
      amountInitial,
      startDateTime
    } = this.state;
    localState = {
      medication,
      dosage,
      dosageUnit,
      frequency,
      minutesBetweenDoses,
      amountInitial,
      startDateTime
    };

    const user = this.props.navigation.getParam("user", {});
    const prescriptionID = this.props.navigation.getParam("prescription", {})
      .id;
    var patient;
    var patientID = null;

    // this view is used by both patnav and docnav
    switch (this.props.navigation.state.routeName) {
      case "PatientEditPrescription":
        patientID = user.id;
        break;
      case "DoctorEditPrescription":
        patient = this.props.navigation.getParam("patient", {});
        patientID = patient.id;
        break;
    }

    localState.patientID = patientID;
    localState.prescriptionID = prescriptionID;
    localState.navigation = this.props.navigation;
    localState.email = user.email;
    localState.password = user.password;

    modifyPrescription(localState);
  };

  render() {
    const prescription = this.props.navigation.getParam("prescription", {});
    var dateTimePickerText = (
      <Text style={styles.buttonText}>Choose a start date </Text>
    );
    if (this.state.startDateTime !== null) {
      dateTimePickerText = (
        <Text style={styles.buttonText}>
          {this.state.startDateTime.toString().slice(0, 25)}
        </Text>
      );
    }

    return (
      <ScrollView style={{ padding: 20 }}>
        <Text style={styles.formSection}>Medication Information</Text>
        <TextInput
          defaultValue={prescription.medication}
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Medication Name"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ medication: value })}
        />
        <TextInput
          defaultValue={prescription.amountInitial.toString()}
          keyboardType="numeric"
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Initial Amount"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value =>
            this.setState({ amountInitial: Number(value) })
          }
        />
        <TextInput
          defaultValue={prescription.dosageSchedule[0].dosage.toString()}
          style={styles.input}
          keyboardType="numeric"
          underlineColorAndroid="transparent"
          placeholder="Dosage"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ dosage: Number(value) })}
        />

        <Picker
          mode="dropdown"
          iosIcon={<Icon name="ios-arrow-down-outline" />}
          placeholder="Dosage Unit"
          textStyle={{ color: "#009CC6" }}
          itemStyle={{
            backgroundColor: "#d3d3d3",
            marginLeft: 0,
            paddingLeft: 10
          }}
          itemTextStyle={{ color: "#788ad2" }}
          selectedValue={this.state.dosageUnit}
          onValueChange={itemValue => this.setState({ dosageUnit: itemValue })}
        >
          <Picker.Item label="Gram" value={DOSAGE_UNIT.GRAM} />
          <Picker.Item label="Liter" value={DOSAGE_UNIT.LITER} />
        </Picker>

        <Picker
          mode="dropdown"
          iosIcon={<Icon name="ios-arrow-down-outline" />}
          placeholder="Dosage Frequency"
          textStyle={{ color: "#009CC6" }}
          itemStyle={{
            backgroundColor: "#d3d3d3",
            marginLeft: 0,
            paddingLeft: 10
          }}
          itemTextStyle={{ color: "#788ad2" }}
          selectedValue={this.state.frequency}
          onValueChange={itemValue => {
            this.setState({ frequency: itemValue });
            this.setState({
              minutesBetweenDoses: convertFrequencyToMins(itemValue)
            });
          }}
        >
          <Picker.Item label="Every Day" value={FREQUENCY.EVERY_DAY} />
          <Picker.Item label="Every Week" value={FREQUENCY.EVERY_WEEK} />
          <Picker.Item
            label="Every Minute (for demoing purposes)"
            value={FREQUENCY.EVERY_MINUTE}
          />
        </Picker>

        <Button
          bordered
          style={styles.button}
          onPress={this._showStartDateTimePicker}
        >
          {dateTimePickerText}
        </Button>
        <DateTimePicker
          mode="datetime"
          isVisible={this.state.isStartDateTimePickerVisible}
          onConfirm={this._handleStartDatePicked}
          onCancel={this._hideStartDateTimePicker}
        />

        <Button style={styles.button} onPress={this.editPrescriptionOnPress}>
          <Text>Save</Text>
        </Button>
        <Button
          danger
          style={styles.button}
          onPress={this.deletePrescriptionOnPress}
        >
          <Text>Delete</Text>
        </Button>
      </ScrollView>
    );
  }
}

const styles = StyleSheet.create({
  formSection: {
    fontSize: 20,
    padding: 5,
    margin: 10,
    color: "black"
  },
  input: {
    margin: 10,
    height: 40,
    padding: 10,
    borderColor: settings.THEME_COLOR,
    borderWidth: 1,
    borderRadius: 7
  },
  button: {
    margin: 10,
    alignItems: "center",
    borderColor: settings.THEME_COLOR
  },
  buttonText: {
    color: settings.THEME_COLOR
  }
});
AppRegistry.registerComponent(
  "EditPrescriptionView",
  () => EditPrescriptionView
);
