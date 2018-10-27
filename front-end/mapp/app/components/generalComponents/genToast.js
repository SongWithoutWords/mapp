import { Toast } from "native-base";
export const genToast = (text, buttonText, duration) => {
  Toast.show({
    text: text,
    buttonText: buttonText,
    duration: duration
  });
};

export default genToast;
