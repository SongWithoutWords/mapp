export default function(spec) {
  spec.describe("Sign up as a patient", function() {
    spec.it("Select user type and fill in accoung info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignUp");
      await spec.pause(500);
      await spec.fillIn("SignUp.TextInput.Email", "ablah@a.com");
      await spec.pause(500);
      await spec.fillIn("SignUp.TextInput.Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.Confirm Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.First Name", "sina");
      await spec.fillIn("SignUp.TextInput.Last Name", "saleh");
      await spec.press("SignUp.Button.SignUp");
      //await spec.pause(2000);
    });
  });

}
