import { FREQUENCY } from "../config/constants";
export const convertFrequencyToMins = freq => {
  switch (freq) {
    case FREQUENCY.EVERY_DAY:
      return 24 * 60;
    case FREQUENCY.EVERY_MINUTE:
      return 1;
    case FREQUENCY.EVERY_WEEK:
      return 7 * 24 * 60;
    default:
      return 0;
  }
};

export const convertMinsToFreqString = mins => {
  switch (mins) {
    case 24 * 60:
      return FREQUENCY.EVERY_DAY;
    case 1:
      return FREQUENCY.EVERY_MINUTE;
    case 7 * 24 * 60:
      return FREQUENCY.EVERY_WEEK;
    default:
      return "every " + mins + " minutes";
  }
};
