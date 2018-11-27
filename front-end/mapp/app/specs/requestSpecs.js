const shortPause = 200;
const longPause = 2050;

////////////////////////// Patient SignIn and make a request /////////////////////////////
export default function(spec) {
  spec.describe("Sign up as a new patient and make a request", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(longPause);
      await spec.fillIn("SignUp.TextInput.Email", makeEmail());
      await spec.fillIn("SignUp.TextInput.Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.Confirm Password", "ablahh");
      await spec.fillIn("SignUp.TextInput.First Name", "testRequest");
      await spec.fillIn("SignUp.TextInput.Last Name", "request");
      await spec.press("SignUp.Button.SignUp");
      await spec.pause(longPause);
      await spec.press("Navigation.DoctorList");
      await spec.pause(longPause);
      await spec.fillIn("DoctorList.SearchBar", "denash");
      await spec.pause(longPause);
      await spec.press("DoctorList.SendRequest.denash");
      await spec.pause(longPause);
      await spec.press("Navigation.Account");
      await spec.pause(longPause);
      await spec.press("Account.Button.SignOut");
      await spec.pause(longPause);
      await spec.exists("Welcome.Doctor");
    });
  });

  //////////////////////////// Doctor SignIn and accept the request/////////////////////////////

  spec.describe("Sign in as a valid Doctor and accept the request", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(longPause);
      await spec.press("SignUp.Button.SignIn");
      await spec.pause(longPause);
      await spec.fillIn("SignIn.TextInput.Email", "docdenash@gmail.com");
      await spec.pause(longPause);
      await spec.fillIn("SignIn.TextInput.Password", "pppppp");
      await spec.pause(longPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(longPause);
      await spec.press("Navigation.Inbox");
      await spec.pause(longPause);
      await spec.press("DoctorInbox.AcceptRequest.testRequest");
      await spec.pause(longPause);
      await spec.press("Navigation.PatientList");
      await spec.pause(longPause);
      await spec.fillIn("PatientList.SearchBar", "testRequest");
      await spec.pause(longPause);
      await spec.exists("PatientList.patient.testRequest");
      await spec.pause(longPause);
      await spec.press("Navigation.Account");
      await spec.pause(longPause);
      await spec.press("Account.Button.SignOut");
      await spec.pause(longPause);
      await spec.exists("Welcome.Doctor");
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
  