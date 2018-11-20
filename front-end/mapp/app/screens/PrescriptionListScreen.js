import React, { Component } from "react";
import {
  Text,
  View,
  TouchableWithoutFeedback,
  AppRegistry,
  ScrollView,
  StyleSheet
} from "react-native";
import { Card, CardItem, Body } from "native-base";
import settings from "../config/settings";
import ActionButton from "react-native-action-button";
import genAlert from "../components/generalComponents/genAlert"

class PrescriptionListScreen extends Component {
  prescriptionOnPress = id => {
    this.props.navigation.navigate("PrescriptionInfo", {
      prescription: this.props.screenProps.prescriptions.byId[id]
    });
  };

  fabOnPress = () => {
    genAlert("Adding a new prescription by patients", "Has not been implemented :(");
  };

  mapPrescriptionToCard = prescrption => {
    const doctor = this.props.screenProps.doctors.byId[prescrption.doctor];
    return (
      <TouchableWithoutFeedback
        onPress={() => this.prescriptionOnPress(prescrption.id)}
        key={prescrption.id}
      >
        <Card>
          <CardItem header bordered>
            <Text style={styles.text}>{prescrption.medication}</Text>
          </CardItem>
          <CardItem bordered>
            <Body>
              <View>
                <Text>Doctor: {doctor.firstName + " " + doctor.lastName}</Text>
                <Text>Dosage Unit: {prescrption.dosageUnit}</Text>
                <Text>Amount Initial: {prescrption.amountInitial}</Text>
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
