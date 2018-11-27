import PushNotification from 'react-native-push-notification';
import settings from "../config/settings";

export function sendNotification(message, subject, id) {
    PushNotification.localNotification({
        /* Android Only Properties */
        id: id, 
        ticker: "Notification for new request",
        autoCancel: true, 
        largeIcon: "ic_launcher", 
        smallIcon: "ic_notification", 
        bigText: message, 
        subText: subject,
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
        userInfo: {id : id},
        /* iOS and Android properties */
        title: subject, 
        message: message, 
        playSound: true, 
        soundName: 'default', 
    });
}