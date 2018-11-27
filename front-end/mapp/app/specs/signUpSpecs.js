const shortPause = 200;
const longPause = 2050;
export default function(spec) {
  spec.describe("Sign up as a patient", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignUp.TextInput.Email", makeEmail());
      await spec.fillIn("SignUp.TextInput.Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.Confirm Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.First Name", "sina");
      await spec.fillIn("SignUp.TextInput.Last Name", "saleh");
      await spec.press("SignUp.Button.SignUp");
      await spec.pause(shortPause);
      await spec.press("Account.Button.SignOut");
      await spec.pause(longPause);
      await spec.exists("Welcome.Patient");
    });
  });
  spec.describe("Sign up as a patient", function() {
    spec.it("Only fill in email", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignUp.TextInput.Email", "ablah@a.com");
      await spec.pause(shortPause);
      await spec.exists("SignUp.TextInput.Password");
      await spec.press("SignUp.Button.SignUp");
      await spec.pause(shortPause);
      await spec.exists("SignUp.TextInput.Password");
    });
  });
  spec.describe("Sign up as a patient", function() {
    spec.it("Only fill in email and password", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignUp.TextInput.Email", "ablah@a.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignUp.TextInput.Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.Confirm Password", "ablahh");
      await spec.press("SignUp.Button.SignUp");
      await spec.pause(shortPause);
      await spec.exists("SignUp.TextInput.Password");
    });
  });
}

function makeEmail() {
  var text = "";
  var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

  for (var i = 0; i < 5; i++)
    text += possible.charAt(Math.floor(Math.random() * possible.length));

  return text + "@gmail.com";
}
