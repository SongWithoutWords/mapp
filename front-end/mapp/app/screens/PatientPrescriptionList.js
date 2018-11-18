import React, { Component } from "react";
import { View, StyleSheet, ScrollView, AppRegistry, Text, TextInput, } from "react-native";
import PrescriptionCardComponent from "../components/cardComponents/PrescriptionCardComponent";
import { hook, wrap } from "cavy";
// or any pure javascript modules available in npm
import { Card } from 'react-native-elements'; // 0.19.1
import { Button, TouchableOpacity, ProgressBar, Dimensions } from 'react-native';
import ProgressBarAnimated from 'react-native-progress-bar-animated';

export default class PatientPrescriptionList extends React.Component {
  /*onPress = () => {
    this.props.navigation.navigate("PrescriptionInfo");
  };*/

  render() {
    return (
      <View style={styles.container}>
        <Card>
        <Text style={styles.medfield}>
          Medication: <Text style={styles.fieldValue}>
             Cefixime 400
          </Text>
        </Text>
        <Text style={styles.medfield}>
          Physician: <Text style={styles.fieldValue}>
             Dr. Saleh
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
        <View>
          <ProgressBarAnimated
            width={barWidth}
            value={80}
            height = {20}
            backgroundColor="#6CC644"
            barAnimationDuration = {0}
          />
          <View style={styles.buttonContainer}>
            <View style={styles.buttonInner}>
              <Text style = {styles.remainingPills}>8/10 Pills</Text>
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
      </View>

    );
  }
}

const barWidth = Dimensions.get('screen').width - 30;
const progressCustomStyles = {
      backgroundColor: 'red',
      borderRadius: 0,
      borderColor: 'orange',
};
const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    //justifyContent: 'center',
    padding: 10,
    backgroundColor: '#ecf0f1',
  },
  fieldValue: {
    fontSize: 16,
    fontWeight: "200",
    fontFamily: 'Poppins',
    textAlign: 'center',
    color: 'black',
  },
  medfield: {
    fontSize: 16,
    fontWeight: '600',
    fontFamily: 'Circular',
    //textAlign: 'center',
    color: '#009CC6',
    marginBottom: 40,
    marginLeft: 24,
    marginRight: 24,
  },
  remainingPills: {
    textAlign: 'center',
    fontSize: 20,
    fontFamily:'Circular',
    fontWeight: '400',
    color: 'black',
  },
  button: {
    margin: 24,
    fontSize: 22,
    fontFamily:'Circular',
    fontWeight: '600',
    width:'30%',
    color: 'white'
    //textAlign: 'center',
    //color: '#34495e',
  },
  buttonText: {
    color : 'white',
    fontFamily: 'Circular',
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
//export default hook(PrescriptionListScreen);
AppRegistry.registerComponent('PatientPrescriptionList', () => PatientPrescriptionList);
