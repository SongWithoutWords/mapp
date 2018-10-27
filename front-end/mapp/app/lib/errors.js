function checkRequestErrors(response) {
  // The ok read-only property of the Response 
  // interface contains a Boolean stating whether 
  // the response was successful (status in the range 200-299) or not.
  if (!response.ok) { 
    if(response.status === 400 || response.status === 403){
      throw Error("Email or password is incorrect, please retry");
    }else{
      let msg = JSON.stringify(response.json());
      throw Error(msg);
    }
  }
  return response;
}

export default checkRequestErrors;
