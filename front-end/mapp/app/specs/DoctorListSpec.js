export default function(spec) {
  spec.describe("Sign in as a patient", function() {
    spec.it("Select user type and fill in accoung info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "PatGavin@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "pppppp");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
    });
  });

}
