import React, { Component } from "react";
import settings from "../config/settings";
import {
  Text,
  View,
  StyleSheet,
  AppRegistry,
  RefreshControl,
  FlatList
} from "react-native";
import { List, ListItem, SearchBar } from "react-native-elements";

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

  // everytime props changes update internal state: patients to
  // conform to the invariant
  componentWillReceiveProps(nextProps) {
    if (
      nextProps.screenProps.user.myPatients !==
      this.props.screenProps.user.myPatients
    ) {
      const myPatientIDs = nextProps.screenProps.user.myPatients;
      const patients = [];
      myPatientIDs.forEach(id => {
        patients.push(nextProps.screenProps.patients.byId[id]);
      });
      this.setState({ patients: patients });
    }
  }

  onPress = id => {
    console.log(id);
    this.props.navigation.navigate("PatientInfo", {
      patient: this.props.screenProps.patients.byId[id],
      user: this.props.screenProps.user
    });
  };

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
          // refreshControl={
          //   <RefreshControl
          //     refreshing={this.props.screenProps.isFetchingUser}
          //     onRefresh={() => {
          //       const { email, password } = this.props.screenProps.user;
          //       const form = { email, password };
          //       const url = settings.REMOTE_SERVER_URL + settings.LOGIN_RES;
          //       this.props.screenProps.onSignIn(url, form);
          //     }}
          //   />
          // }
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
