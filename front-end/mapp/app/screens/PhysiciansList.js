import React, { Component } from 'react';
import { SafeAreaView } from 'react-navigation'; // for iphonex

import {
  Text,
  View,
  ScrollView
} from 'react-native';
import { List, ListItem } from 'react-native-elements';

const fake_physicians = [
  {
    name: 'Hot dog',
    avatar_url: 'https://s3.amazonaws.com/uifaces/faces/twitter/ladylexy/128.jpg',
    subtitle: 'Vice President'
  },
  {
    name: 'Not Hot Dog',
    avatar_url: 'https://s3.amazonaws.com/uifaces/faces/twitter/adhamdannaway/128.jpg',
    subtitle: 'Vice Vice President'
  },
]

class PhysicianList extends Component {
  render(){return(
    <SafeAreaView>
        <ScrollView>
          <List>
            {fake_physicians.map((physician, i) => (
              <ListItem
                key={i}
                roundAvatar
                avatar={{ uri: physician.avatar_url }}
                title={physician.name}
                subtitle={physician.subtitle}
              />
            ))}
          </List>
        </ScrollView>
    </SafeAreaView>
  )}
  }
  
  export default PhysicianList;