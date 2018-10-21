import React from 'react';
import { createBottomTabNavigator} from 'react-navigation';
import { createMaterialBottomTabNavigator } from 'react-navigation-material-bottom-tabs';
import  Ionicons  from 'react-native-vector-icons/Ionicons';
import  MaterialCommunityIcons  from 'react-native-vector-icons/MaterialCommunityIcons';
import  AntDesign  from 'react-native-vector-icons/AntDesign';

import DoctorListScreen from '../screens/DoctorListScreen';
import PrescriptionListScreen from '../screens/PrescriptionListScreen';

export const Tabs = createMaterialBottomTabNavigator(
    {
      // screens and their navigation options
      PrescriptionList: { 
        screen: PrescriptionListScreen,
        navigationOptions: {
          tabBarLabel: 'Prescriptions',
          tabBarIcon: ({ tintColor }) => (
            <MaterialCommunityIcons name="pill" size={25} color={tintColor} />
          )
        }
      },
      DoctorList: {
        screen: DoctorListScreen,
        navigationOptions: {
          tabBarLabel: 'Doctors',
          tabBarIcon: ({ tintColor }) => (
            <MaterialCommunityIcons name="doctor" size={25} color={tintColor} />
          )
        }
      }
    }, {
      initialRouteName: 'PrescriptionList',
      shifting: true
    })

