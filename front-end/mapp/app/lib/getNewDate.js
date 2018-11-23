export function getNewDate(first, timeBetweenDoses){
    firstDate = new Date(first);
    timeNow = new Date();
    d = Math.ceil((timeNow - firstDate) / (timeBetweenDoses * 60000));
    time = new Date(firstDate.getTime() + (d * (timeBetweenDoses * 60000)));
    return time;
}