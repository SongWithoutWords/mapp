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
import postData from "../lib/postData";
import genAlert from "../components/generalComponents/genAlert";
import DateTimePicker from "react-native-modal-datetime-picker";
import { Picker, Header, Content, Button, Text } from "native-base";
import { FREQUENCY } from "../config/constants";

export default class MakePrescriptionView extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      isStartDateTimePickerVisible: false,
      medication: "",
      dosage: 0,
      dosageUnit: "",
      frequency: "", // used to compute minutes between doses
      minutesBetweenDoses: 0,
      amountInitial: 0,
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

  convertFrequencyToMins = freq => {
    switch (freq) {
      case FREQUENCY.EVERY_DAY:
        return 24 * 60;
      case FREQUENCY.EVERY_MINUTE:
        return 1;
      case FREQUENCY.EVERY_WEEK:
        return 7 * 24 * 60;
      default:
        return 0;
    }
  };

  createPrescriptionOnPress = () => {
    const patient = this.props.navigation.getParam("patient", {});
    const user = this.props.navigation.getParam("user", {});
    const url = settings.REMOTE_SERVER_URL + settings.PRESCRIPTION_RES;
    const dosageSchedule = [];

    var schedule = {};
    schedule.firstDose = this.state.startDateTime;
    schedule.dosage = this.state.dosage;
    schedule.minutesBetweenDoses = this.state.minutesBetweenDoses;
    dosageSchedule.push(schedule);

    const json = {
      patient: patient.id,
      doctor: user.id,
      medication: this.state.medication,
      dosageUnit: this.state.dosageUnit,
      amountInitial: this.state.amountInitial,
      dosageSchedule: dosageSchedule
    };

    console.log(JSON.stringify(json));

    return postData(url, json)
      .then(response => {
        this.props.navigation.goBack();
      })
      .catch(error => {
        genAlert("Failed to create a new prescription", error.message);
      });
  };

  render() {
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
          style={styles.input}
          underlineColorAndroid="transparent"
          placeholder="Medication Name"
          placeholderTextColor="#009CC6"
          autoCapitalize="none"
          onChangeText={value => this.setState({ medication: value })}
        />
        <TextInput
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
          <Picker.Item label="Gram" value="Gram" />
          <Picker.Item label="Liter" value="Liter" />
        </Picker>

        <Picker
          mode="dropdown"
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
            this.setState({ minutesBetweenDoses: this.convertFrequencyToMins(itemValue) });
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

        <Button style={styles.button} onPress={this.createPrescriptionOnPress}>
          <Text>Create a new prescription</Text>
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
  "MakePrescriptionView",
  () => MakePrescriptionView
);
