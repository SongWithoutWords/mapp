import moment from "moment";
export function getLocalDateTimeString(isoDateString){
    return moment(isoDateString).format("MMM Do YYYY, h:mm a")
}
