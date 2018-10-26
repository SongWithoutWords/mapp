import { Alert } from "react-native";
export const genAlert = (msg) => {
        Alert.alert(
          "Sign up failed",
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