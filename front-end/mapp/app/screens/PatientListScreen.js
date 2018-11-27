import React, { Component } from "react";
import settings from "../config/settings";
import _ from "lodash"
import {
  Text,
  View,
  StyleSheet,
  AppRegistry,
  RefreshControl,
  FlatList,
  PushNotificationIOS,
  Platform,
} from "react-native";
import { List, ListItem, SearchBar } from "react-native-elements";
import {setupPushNotification} from "../lib/setupPushNotification"
import {sendNotification} from "../lib/sendNotification";

// note that this class now is actually "my patients" screen
// cuz unlike doctor list screen, patients' info can only accessed
// by their doctors
class PatientListScreen extends Component {
  constructor(props) {
    super(props);

    // get patient array
    const myPatientIDs = this.props.screenProps.user.myPatients;
    const patients = [];
    myPatientIDs.forEach(id => {
      patients.push(this.props.screenProps.patients.byId[id]);
    });

    this.state = {
      patients: patients
      // state used by search bar
      // an array of patients connected to this doctor
      // invariant: consistent with the patient states in redux store
    };
  }

  componentWillMount(){
    this.pushNotification = setupPushNotification(this.handleNotificationOpen);
    if(Object.keys(this.props.screenProps.pendingRequests.allIds).length > 0)
      sendNotification(
        "You have a new Patient requesting to connect to your account. Press here to go to the inbox screen",
        "You have a new Patient request!",
        this.props.screenProps.user.id + ""
      );
    this.sendRenewalNotifications();
  }

  // every time props changes update internal state: patients to
  // conform to the invariant
  componentWillReceiveProps(nextProps) {
    if (_.isEqual(nextProps.screenProps.user.myPatients, this.props.screenProps.user.myPatients))
      console.log("myPatients are equal");
    else{
      const myPatientIDs = nextProps.screenProps.user.myPatients;
      const patients = [];
      myPatientIDs.forEach(id => {
        patients.push(nextProps.screenProps.patients.byId[id]);
      });
      this.setState({ patients: patients });
    }
  }
   
  // if new patient request send notification
  componentDidUpdate(prevProps) {
    if (_.isEqual(prevProps.screenProps.user.myPendingRequests, this.props.screenProps.user.myPendingRequests))
      console.log("Pending requests are equal");
    else{
      console.log("DEBUG: " + prevProps.screenProps.user.myPendingRequests.length + " " + this.props.screenProps.user.myPendingRequests.length);
      if(this.props.screenProps.user.myPendingRequests.length - prevProps.screenProps.user.myPendingRequests.length > 0)
        sendNotification(
          "You have a new Patient requesting to connect to your account. Press here to go to the inbox screen",
          "You have a new Patient request!",
          this.props.screenProps.user.id + ""
        );
    }

    if (_.isEqual(prevProps.screenProps.patients, this.props.screenProps.patients))
      console.log("Patients are equal");
    else
      this.sendRenewalNotifications();
  }

  sendRenewalNotifications = () => {
    this.props.screenProps.user.myPatients.map( id => {
      this.props.screenProps.patients.byId[id].prescriptions.forEach( prescription => {
        numberLeft = prescription.amountInitial / prescription.dosageSchedule[0].dosage -
        Object.keys(prescription.dosesTaken).length;
        if(numberLeft < 2 && prescription.doctor === this.props.screenProps.user.id)
          sendNotification(
            "Your patient " + this.props.screenProps.patients.byId[prescription.patient].firstName + " " + 
            this.props.screenProps.patients.byId[prescription.patient].lastName + " is running low on " + 
            prescription.medication + ". please go to the patients page if you want to renew the prescription.",
            "You might need to renew a prescription",
            id + "" + this.props.screenProps.user.id + ""
          );
      });
    });
  }

  onPress = id => {
    console.log(id);
    this.props.navigation.navigate("PatientInfo", {
      patient: this.props.screenProps.patients.byId[id],
      user: this.props.screenProps.user
    });
  };

  handleNotificationOpen = (notification) => {
    if(notification.title === "You might need to renew a prescription")
      this.props.navigation.navigate("PatientList");
    else
      this.props.navigation.navigate("Inbox");
    if(Platform.OS === 'ios')
      notification.finish(PushNotificationIOS.FetchResult.NoData);
  }

  renderHeader = () => {
    return (
      <SearchBar
        round
        placeholder="Type Here..."
        lightTheme
        onChangeText={text => this.searchFilterFunction(text)}
        onClearText={() => this.searchFilterFunction("")}
        autoCorrect={false}
        clearIcon
      />
    );
  };

  renderSeparator = () => {
    return (
      <View
        style={{
          height: 1,
          width: "100%",
          backgroundColor: "#CED0CE"
        }}
      />
    );
  };

  renderItem = ({ item }) => {
    return (
      <View style={{ backgroundColor: "white" }}>
        <ListItem
          roundAvatar
          title={`${item.firstName} ${item.lastName}`}
          subtitle={item.email}
          containerStyle={{ borderBottomWidth: 0 }}
          onPress={() => this.onPress(item.id)}
          style={{
            backgroundColor: "white"
          }}
        />
      </View>
    );
  };

  searchFilterFunction = text => {
    const myPatientIDs = this.props.screenProps.user.myPatients;
    const patients = [];
    myPatientIDs.forEach(id => {
      patients.push(this.props.screenProps.patients.byId[id]);
    });
    const filteredPatients = patients.filter(patient => {
      const patientNameData = `${patient.firstName.toUpperCase()} ${patient.lastName.toUpperCase()}`;
      const textData = text.toUpperCase();
      return patientNameData.indexOf(textData) > -1; // check if query text is found inside patient's name
    });
    this.setState({ patients: filteredPatients });
  };

  render() {
    return (
      <View style={styles.container}>
        <FlatList
          data={this.state.patients}
          renderItem={this.renderItem}
          keyExtractor={item => item.id.toString()}
          ItemSeparatorComponent={this.renderSeparator}
          ListHeaderComponent={this.renderHeader}
        />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1
  }
});

export default PatientListScreen;
AppRegistry.registerComponent("PatientListScreen", () => PatientListScreen);
