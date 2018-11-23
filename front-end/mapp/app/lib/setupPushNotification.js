import PushNotification from 'react-native-push-notification';

export function setupPushNotification(handleNotification) {
  PushNotification.configure({

      onNotification: function(notification) {
        console.log('NOTIFICATION:', notification);
        handleNotification(notification);
      },

      popInitialNotification: true,
      requestPermissions: true,
  })

  return PushNotification
}