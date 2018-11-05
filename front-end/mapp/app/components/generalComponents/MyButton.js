import { Button } from "react-native-elements";
import React, { Component } from "react";
import settings from "../../config/settings";
import { hook } from "cavy";

const 
class MyButton extends Component {
  render() {
    console.log("Button." + this.props.title);
    return (
      <Button
        // ref={this.props.generateTestHook("Button." + this.props.title)}
        buttonStyle={{ marginTop: 20 }}
        backgroundColor={settings.THEME_COLOR}
        title={this.props.title}
        onPress={this.props.onPress}
      />
    );
  }
}

const TestableButton = hook(MyButton);
export default TestableButton;
// export default hook(MyButton);
