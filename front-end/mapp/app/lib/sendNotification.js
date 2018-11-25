import PushNotification from 'react-native-push-notification';
import settings from "../config/settings";

export function sendNotification() {
    PushNotification.localNotification({
        /* Android Only Properties */
        id: "1", 
        ticker: "Notification for new request",
        autoCancel: true, 
        largeIcon: "ic_launcher", 
        smallIcon: "ic_notification", 
        bigText: "You have a new Patient requesting to connect to your account. Press here to go to the inbox screen", 
        subText: "You have a new Patient request!",
        color: settings.THEME_COLOR, 
        vibrate: true, 
        vibration: 300, // vibration length in milliseconds, ignored if vibrate=false, default: 1000
        tag: 'mapp_request_notification', 
        group: "request_notification", 
        ongoing: false, // (optional) set whether this is an "ongoing" notification
        priority: "high", // (optional) set notification priority, default: high
        visibility: "public", // (optional) set notification visibility, default: private
        importance: "high", // (optional) set notification importance, default: high
        /* ios only */
        userInfo: {id : "1"},
        /* iOS and Android properties */
        title: "You have a new Patient request!", 
        message: "You have a new Patient requesting to connect to your account. Press here to go to the inbox screen", 
        playSound: true, 
        soundName: 'default', 
    });
}