#' classify_lake
#'
#' classify pollution risk for a lake
#' @param nutrient concentraion (mg/L) (at least 5 days)
#' @param temperature (C) (at least 5 days)
#' @param nutrient_threshold_med (mg/L) (default 5)
#' @param nutrient_threshold_high (mg/L) (default 8)
#' @param temperature_threshold (mg/L) (default 10)
#' @return risk (high,med, low), mean nutrient concentration (mg/L)


classify_lake = function(nutrient, temperature, nutrient_threshold_med=5,
                         nutrient_threshold_high=8, temperature_threshold=10) {

  # start with some error checking
  if (length(nutrient) < 5)
    return("Not enough nutrient measurements, at least 5 are needed")

  if (length(temperature) < 5)
      return("Not enought temperature measurements, at least 5 are needed")


  # a while loop is useful here
  # sets the number of days equal to 0
  numhotday = 0
  i=1
  # we use while here because we want to exit our loop any time we get more than 5 days with
  # air temperature greater than the threshold

  # go until the number of days is less than 5 and also make sure that the counter is less than the number of days you have
  while ( (numhotday < 5) && (i <= length(temperature))) {
    
    # use an if statement to see if the temperature on day i is greater than the temp of the threshold
    # then add a day to the number of hot days
      if (temperature[i] > temperature_threshold)
        # we have another day with greater than 10 so accumulate
        numhotday = numhotday+1
      else
        # we have to start over
        numhotday = 0
      # remember with while loops we need to increment our counter
      
      # add 1 to i through all the days in your temperature record
      i = i+1

  }

  # compute the mean nutrient
  mean_nutrient = mean(nutrient)

  # only high or med if temperature has been 10 degrees for more than 5 days
  
  # if there's a number of days that's hot greater than 5, then classify it
  if (numhotday >= 5) {
    
    # used the case_when statement to say when mean is less than median then it's low
   risk= case_when ( mean_nutrient < nutrient_threshold_med ~ "low",
                     
                     # if it's greater or equal to the median and the mean is less than the high it's medium
                     mean_nutrient >= nutrient_threshold_med &
                       mean_nutrient < nutrient_threshold_high ~ "med",
                     
                     # if it's higher than the mean or equal to the high then it's high
                mean_nutrient >= nutrient_threshold_high ~ "high" )
   
   # if there's not 5 hot days in a row, then the risk is low
  } else
    risk = "low"


  return(list(risk=risk, mean_nutrient=mean_nutrient))
}

