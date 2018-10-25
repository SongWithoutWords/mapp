// called when sign in or sign up
export const onSignIn = () => {
  console.log("handle sign in");
};

export const onSignUp = () => {
  console.log("handle sign up");
};

// called when sign out
export const onSignOut = () => {
  console.log("handle sign out");
};

  // onPress = () => {
  //   return fetch(settings.REMOTE_SERVER_URL + settings.PATIENT_RES, {
  //     method: "POST",
  //     headers: {
  //       Accept: "application/json",
  //       "Content-Type": "application/json"
  //     },
  //     body: JSON.stringify({
  //       email: this.state.email,
  //       password: this.state.password,
  //       firstName: this.state.firstName,
  //       lastName: this.state.lastName
  //     })
  //   })
  //     .then(response => response.json())
  //     .then(responseJson => {
  //       this.setState(
  //         {
  //           isLoading: false,
  //           dataSource: responseJson
  //         },
  //         function() {
  //           Alert.alert(
  //             "Created a patient account",
  //             JSON.stringify(this.state.dataSource),
  //             [
  //               {
  //                 text: "Cancel",
  //                 onPress: () => console.log("Cancel Pressed"),
  //                 style: "cancel"
  //               },
  //               { text: "OK", onPress: () => console.log("OK Pressed") }
  //             ],
  //             { cancelable: false }
  //           );
  //           this.props.navigation.navigate("Main");
  //         }
  //       );
  //     })
  //     .catch(error => {
  //       console.error(error);
  //     });
  // };