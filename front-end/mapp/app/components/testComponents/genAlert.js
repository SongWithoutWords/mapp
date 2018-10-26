import { Alert } from "react-native";
export const genAlert = (title, msg) => {
        Alert.alert(
          title,
          msg,
          [
            {
              text: "Cancel",
              onPress: () => console.log("Cancel Pressed"),
              style: "cancel"
            },
            { text: "OK", onPress: () => console.log("OK Pressed") }
          ],
          { cancelable: false }
        );
}

export default genAlert;