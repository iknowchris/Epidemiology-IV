# Truncate a dataset with censoring indicators to change the end time
truncateSurvivalTime <- function(event_time = NULL, censor_indicator = NULL, truncate_time = NULL){
  # assumes that the censor indicator variable = 1 if censored
  df.tmp <- data.frame(cbind(event_time, censor_indicator))
  df.tmp$trunc_time <- ifelse(df.tmp$event_time < truncate_time, df.tmp$event_time, truncate_time)
  df.tmp$trunc_event <- 0
  df.tmp$trunc_event <- ifelse((df.tmp$censor_indicator == 1) & (df.tmp$event_time <= df.tmp$trunc_time), 1, df.tmp$trunc_event)
  return(df.tmp)
}

