import settings from "../config/settings";
export function genTabNavOptions(initialRouteName, order) {
  let options = {
    initialRouteName: initialRouteName,
    shifting: true,
    activeColor: settings.ACTIVE_COLOR,
    inactiveColor: settings.INACTIVE_COLOR,
    barStyle: { backgroundColor: settings.THEME_COLOR },
    tabBarOptions: {
      showIcon: true,
      labelStyle: { fontSize: 10 }
    },
    order: order
  };
  return options;
}
