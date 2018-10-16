import React, { Component } from 'react';
import { SafeAreaView } from 'react-navigation'; // for iphonex

import {
  Text,
  View,
  ScrollView
} from 'react-native';
import { List, ListItem } from 'react-native-elements';

const fake_prescriptions = [
  {
    name: 'divalproex',
    avatar_url: 'https://bit.ly/2yhPJY0',
    subtitle: '..'
  },
  {
    name: 'divalproex',
    avatar_url: 'https://bit.ly/2yhPJY0',
    subtitle: '..'
  },
]

class PrescriptionList extends Component {
  render(){return(
    <SafeAreaView>
        <ScrollView>
          <List>
            {fake_prescriptions.map((prescription, i) => (
              <ListItem
                key={i}
                roundAvatar
                avatar={{ uri: prescription.avatar_url }}
                title={prescription.name}
                subtitle={prescription.subtitle}
              />
            ))}
          </List>
        </ScrollView>
    </SafeAreaView>
  )}
  }
  
  export default PrescriptionList;