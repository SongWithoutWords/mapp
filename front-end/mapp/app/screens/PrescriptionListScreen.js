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

class PrescriptionListScreen extends Component {
  onPress = id => {
    this.props.navigation.navigate("PrescriptionInfo", {
      prescription: this.props.screenProps.prescriptions.byId[id],
    });
  };

  mapPrescriptionToCard = prescrption =>{
    const doctor = this.props.screenProps.doctors.byId[prescrption.doctor];
    return <TouchableWithoutFeedback onPress={() => this.onPress(prescrption.id)} key={prescrption.id}>
      <Card>
        <CardItem header bordered>
          <Text style={styles.text}>{prescrption.medication}</Text>
        </CardItem>
        <CardItem bordered>
          <Body>
            <View>
              <Text>Doctor: {doctor.firstName + ' ' + doctor.lastName}</Text>
              <Text>Dosage Unit: {prescrption.dosageUnit}</Text>
              <Text>Amount Initial: {prescrption.amountInitial}</Text>
            </View>
          </Body>
        </CardItem>
      </Card>
    </TouchableWithoutFeedback>
  } 

  render() {
    const prescriptions = this.props.screenProps.prescriptions;
    const prescriptionIDs = this.props.screenProps.user.myPrescriptions;
    return (
      <View style={{flex: 1}}>
        <ScrollView style={{flex: 1}}>
          {prescriptionIDs.map(id =>
            this.mapPrescriptionToCard(prescriptions.byId[id])
          )}
        </ScrollView>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  text: {
    color: "#694fad",
    fontSize: 20
  }
});
export default PrescriptionListScreen;
AppRegistry.registerComponent(
  "PrescriptionListScreen",
  () => PrescriptionListScreen
);
