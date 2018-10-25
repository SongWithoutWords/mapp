import React, { Component } from 'react';
import { StyleSheet } from "react-native";

import {
  Text,
  View,
  ScrollView
} from 'react-native';
import { List, ListItem } from 'react-native-elements';

class AccountScreen extends Component {
  render(){
    return(
      <View>
        <ScrollView>
          <List>
          </List>
        </ScrollView>
      </View>
  )}
  }
  
// styles for this screen
const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center"
  }
})
 
export default AccountScreen;