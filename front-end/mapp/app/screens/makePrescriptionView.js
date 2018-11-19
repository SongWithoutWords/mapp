import * as React from 'react';
import { Text, View, StyleSheet, TextInput, ScrollView, Button, TouchableOpacity, AppRegistry } from 'react-native';
// or any pure javascript modules available in npm
import { Card, CheckBox } from 'react-native-elements'; // 0.19.1
import settings from "../config/settings";

export default class MakePrescriptionView extends React.Component {
  /*data PostPrescription = PostPrescription
  { doctor :: DoctorId
  , patient :: PatientId
  , medication :: Text
  , dosageUnit :: DosageUnit
  , amountInitial :: Double
  , dosageSchedule :: [PostRecurringDose]
} deriving(Generic)*/
  constructor(props) {
    super(props);
    this.state = {
       medName: '',
       medDoseUnit: '',
       medInitialAmount : 0,
       startDate : new Date(),
       endDate : new Date(),
       medFreq: ''
    };
  }

  render() {
    return (
      <ScrollView style={{padding: 20,}}>
          <Text
            style={{fontSize: 27, marginLeft:15}}>
            New Prescription
          </Text>
          <Text
            style={styles.formSection}>
            Medication Information
          </Text>
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholder = "Medication Name"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText={value => this.setState({ medName: value })}/>
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholder = "Dosage Unit"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText={value => this.setState({ medDoseUnit: value })}/>
          <TextInput style = {styles.input}
            underlineColorAndroid = "transparent"
            placeholder = "Initial Amount"
            placeholderTextColor = "#009CC6"
            autoCapitalize = "none"
            onChangeText={value => this.setState({ medInitialAmount: value })}/>
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholder = "Start Date"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText={value => this.setState({ startDate: value })}/>
          <TextInput style = {styles.input}
              underlineColorAndroid = "transparent"
              placeholder = "End Date"
              placeholderTextColor = "#009CC6"
              autoCapitalize = "none"
              onChangeText={value => this.setState({ endDate: value })}/>
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholder = "Frequency"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText={value => this.setState({ medFreq: value })}/>
          <Text
            style={styles.formSection}>
            Patient Information
          </Text>
          <Text style={styles.medfield}>
            Patient Name    <Text style={styles.fieldValue}>
             Sina Saleh
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Patient ID    <Text style={styles.fieldValue}>
             50060070
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Date of Birth    <Text style={styles.fieldValue}>
             10/8/1997
            </Text>
          </Text>
          <Text
            style={styles.formSection}>
            Contact Information
          </Text>
          <Text style={styles.medfield}>
            Email Address    <Text style={styles.fieldValue}>
             sina@sina.com
            </Text>
          </Text>
          <Text style={styles.medfield}>
             Phone    <Text style={styles.fieldValue}>
             777-778-8690
            </Text>
          </Text>
          </ScrollView>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center',
    paddingTop: 10,
    backgroundColor: '#ecf0f1',
  },
  formSection:{
    fontSize: 20,
    padding: 5 ,
    margin:15,
    color: "black"
  },
  paragraph: {
    margin: 24,
    fontSize: 18,
    fontWeight: 'bold',
    textAlign: 'center',
    color: '#009CC6',
  },
  input: {
      margin: 15,
      height: 40,
      padding : 10,
      borderColor: '#009CC6',
      borderWidth: 1,
      borderRadius: 7
   },
   fieldValue: {
     fontSize: 16,
     fontWeight: "200",
     //fontFamily: 'Poppins',
     textAlign: 'center',
     color: 'black',
   },
   medfield: {
     fontSize: 16,
     fontWeight: '600',
     //fontFamily: 'Circular',
     //textAlign: 'center',
     color: '#009CC6',
     marginBottom: 40,
     marginLeft: 24,
     marginRight: 24,
   },
   submitButton: {
      backgroundColor: '#009CC6',
      padding: 10,
      margin: 15,
      height: 40,
      borderRadius: 10,
      alignItems:'center',
      width:'25%'
   },
   submitButtonText:{
      color: 'white'
   }
});
AppRegistry.registerComponent('MakePrescriptionView', () => MakePrescriptionView);
