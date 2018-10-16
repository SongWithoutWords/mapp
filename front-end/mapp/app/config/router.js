import React from 'react';
import { createBottomTabNavigator} from 'react-navigation';
import { createMaterialBottomTabNavigator } from 'react-navigation-material-bottom-tabs';
import Ionicons from 'react-native-vector-icons/Ionicons';
import { Icon } from 'react-native-elements';

import PhysicianList from '../screens/PhysiciansList';
import PrescriptionList from '../screens/PrescriptionList';

// TabNavigator takes the configuration and render tabs
// each key represents a tab (it is actually the default title of that tab)
// screen is the component that actually gets rendered for that tab
// we need to export this as a const and put it in the app.js
export const Tabs = createBottomTabNavigator(
    {
      PrescriptionList: PrescriptionList,
      PhysicianList: PhysicianList,
    },
    {
      navigationOptions:
      { 
        tabBarIcon: ({ tintColor }) => <Icon name="account-circle" size={25} color={tintColor} />,
        tabBarOptions: {
            activeTintColor: 'tomato',
            inactiveTintColor: 'gray',
        },
      }
    }
  );

