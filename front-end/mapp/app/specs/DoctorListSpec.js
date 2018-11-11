const shortPause = 200;
const longPause = 2000;
export default function(spec) {
  spec.describe("Sign in as a valid patient with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "PatGavin@gmail.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "pppppp");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("PrescriptionCardComponent");
      await spec.pause(shortPause);
    });
  });

  spec.describe("Sign in as a valid patient with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "PatGavin@gmail.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(shortPause);
    });
  });

  spec.describe("Sign in as an invalid patient with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "PatInvalid@gmail.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "pppppp");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(shortPause);
    });
  });

  spec.describe("Sign in as an invalid patient with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "invalid@email.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(shortPause);
    });
  });

  ////////////////////////// Doctor SignIn /////////////////////////////

  spec.describe("Sign in as a valid Doctor with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "test@gmail.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "testing");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
    });
  });

  spec.describe("Sign in as a valid Doctor with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "test@gmail.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(shortPause);
    });
  });

  spec.describe("Sign in as an invalid Doctor with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "PatInvalid@gmail.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "testing");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(shortPause);
    });
  });

  spec.describe("Sign in as an invalid Doctor with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Email", "invalid@email.com");
      await spec.pause(shortPause);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(shortPause);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(shortPause);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(shortPause);
    });
  });
  
}
