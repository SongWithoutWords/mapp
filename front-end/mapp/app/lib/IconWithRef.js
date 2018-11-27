import React from "react";
import  { Component } from "react";
export default class IconWithRef extends Component {
    render() {
      const { navigation, generateTestHook, routeName, ...rest } = this.props;
      const IconType = this.props.iconType;
      return <IconType ref={generateTestHook(`Navigation.${routeName}`)} onPress={() => navigation.navigate(routeName)} {...rest}/>
    }
}

  