import * as React from 'react';
import { Text, View, StyleSheet, TextInput, ScrollView, Button, TouchableOpacity, AppRegistry } from 'react-native';
// or any pure javascript modules available in npm
import { Card, CheckBox } from 'react-native-elements'; // 0.19.1

export default class MakePrescriptionView extends React.Component {
     state = {
      medName: '',
      medDose: '',
      medFreq: '',
   }
   handleName = (text) => {
      this.setState({ medName: text })
   }
   handleDose = (text) => {
      this.setState({ medDose: text })
   }
   handleFreq = (text) => {
      this.setState({ medFreq: text })
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
             onChangeText = {this.handleName}/>
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholder = "Medication Dosage"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText = {this.handleDose}/>
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholder = "Medication Frequency"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText = {this.handleFreq}/>
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
            Patient SSN    <Text style={styles.fieldValue}>
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
            Mailing and Contact Information
          </Text>
          <Text style={styles.medfield}>
            Mailing Address    <Text style={styles.fieldValue}>
             2415 E 29th Ave Vancouver BC
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Home Phone    <Text style={styles.fieldValue}>
             66677886
            </Text>
          </Text>
          <Text style={styles.medfield}>
            Cell Phone    <Text style={styles.fieldValue}>
             777-778-8690
            </Text>
          </Text>
          <Text
            style={{fontSize: 15, padding: 5 , marginLeft:15}}>
            Medication Allergies
          </Text>
          <CheckBox
            title='Other (specify)'
            checked={this.state.checked}
            checkedColor = '#009CC6'
            onPress={() => this.setState({checked: !this.state.checked})}
          />
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText = {this.handlePhID}/>
          <Text
            style={{fontSize: 15, padding: 5 , marginLeft:15}}>
            Health Conditions
          </Text>
          <CheckBox
            title='Other (specify)'
            checked={this.state.checked}
            checkedColor = '#009CC6'
            onPress={() => this.setState({checked: !this.state.checked})}
          />
          <TextInput style = {styles.input}
             underlineColorAndroid = "transparent"
             placeholderTextColor = "#009CC6"
             autoCapitalize = "none"
             onChangeText = {this.handlePhID}/>

          <TouchableOpacity
             style = {styles.submitButton}
             onPress = {
                () => this.login(this.state.email, this.state.password)
             }>
             <Text style = {styles.submitButtonText}> Sign up </Text>
          </TouchableOpacity>

          <Text
            style={{fontSize: 18, color: 'red', padding: 5 , marginLeft:15}}>
            {this.state.message}
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
