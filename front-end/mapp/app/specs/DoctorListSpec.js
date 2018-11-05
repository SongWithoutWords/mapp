export default function(spec) {
  spec.describe("Sign in as a valid patient with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "PatGavin@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "pppppp");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("PrescriptionCardComponent");
      await spec.pause(2000);
    });
  });

  spec.describe("Sign in as a valid patient with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "PatGavin@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(2000);
    });
  });

  spec.describe("Sign in as an invalid patient with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "PatInvalid@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "pppppp");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(2000);
    });
  });

  spec.describe("Sign in as an invalid patient with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Patient");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "invalid@email.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(2000);
    });
  });

  ////////////////////////// Doctor SignIn /////////////////////////////

  spec.describe("Sign in as a valid Doctor with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "test@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "testing");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("PrescriptionCardComponent");
      await spec.pause(2000);
    });
  });

  spec.describe("Sign in as a valid Doctor with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "test@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(2000);
    });
  });

  spec.describe("Sign in as an invalid Doctor with valid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "PatInvalid@gmail.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "testing");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(2000);
    });
  });

  spec.describe("Sign in as an invalid Doctor with invalid password", function() {
    spec.it("Select user type and fill in account info", async function() {
      await spec.press("Welcome.Doctor");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Email", "invalid@email.com");
      await spec.pause(1500);
      await spec.fillIn("SignIn.TextInput.Password", "invalid");
      await spec.pause(1500);
      await spec.press("SignIn.Button.SignIn");
      await spec.pause(2000);
      await spec.exists("SignIn.TextInput.Email");
      await spec.pause(2000);
    });
  });
  
}
