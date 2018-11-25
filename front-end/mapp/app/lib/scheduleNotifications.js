import PushNotification from 'react-native-push-notification';
import settings from "../config/settings";

export function scheduleNotifications(prescription) {  
    const numberLeft = (prescription.amountInitial / prescription.dosageSchedule[0].dosage) - Object.keys(prescription.dosesTaken).length;
    const firstDate = new Date(prescription.dosageSchedule[0].firstDose);
    const timeNow = new Date();
    d = Math.ceil((timeNow - firstDate) / (prescription.dosageSchedule[0].minutesBetweenDoses * 60000));
    newTime = new Date(firstDate.getTime() + (d * (prescription.dosageSchedule[0].minutesBetweenDoses * 60000)));
    console.log(numberLeft + " New time for new notification " + newTime);
    PushNotification.cancelLocalNotifications({id: prescription.id + ""});
    if(numberLeft > 0){
      PushNotification.localNotificationSchedule({
        /* Android Only Properties */
        id: prescription.id + "", 
        ticker: "Notification to take prescription",
        autoCancel: true, 
        largeIcon: "ic_launcher", 
        smallIcon: "ic_notification", 
        bigText: "It is time to take " + prescription.medication + " press to goto inbox screen", 
        subText: "Time to take your medicine!",
        color: settings.THEME_COLOR, 
        vibrate: true, 
        vibration: 300, // vibration length in milliseconds, ignored if vibrate=false, default: 1000
        tag: 'mapp_prescription_notification', 
        group: "prescription_notification", 
        ongoing: false, // (optional) set whether this is an "ongoing" notification
        priority: "high", // (optional) set notification priority, default: high
        visibility: "public", // (optional) set notification visibility, default: private
        importance: "high", // (optional) set notification importance, default: high
        /* ios only */
        userInfo: {id : prescription.id + ""},
        /* iOS and Android properties */
        title: "Time to take your medicine!", 
        message: "It is time to take " + prescription.medication + " press to goto inbox screen", 
        playSound: true, 
        soundName: 'default', 
        number: numberLeft, // (optional) Valid 32 bit integer specified as string. default: none (Cannot be zero)
        repeatType: 'time', // every repeatTime ms run the notification again 
        repeatTime: prescription.dosageSchedule[0].minutesBetweenDoses * 60000,
        date: newTime
      });
    }
}