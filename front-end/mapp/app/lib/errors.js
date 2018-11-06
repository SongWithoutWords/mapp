function checkRequestErrors(response) {
  // The ok read-only property of the Response 
  // interface contains a Boolean stating whether 
  // the response was successful (status in the range 200-299) or not.
  if (!response.ok) { 
     throw Error(response.status + " Server response is bad");
  }
  return response;
}

export default checkRequestErrors;
