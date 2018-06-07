ConvertHourtoTP <- function(hour, AM = c(7:9),IP = c(10:15),PM = c(16:18)){
  # Converts a vector of hours to TP to store in a new column.
  # args:
  # hour: vector of hours, numeric
  # AM, IP, PM, vectors with the hours to convert to each TP, default values provided
  # 
  # returns:
  # vector of TP to add to the original dataset
  data <- data.frame(hour)
  data$TP <- "Other"
  tp_list <- list(
    "1" = AM,	
    "3" = PM,	
    "2" = IP)
  for (i in 1:3) {
    data[data$hour %in% tp_list[[i]],]$TP <- names(tp_list)[i] 
  }
  return(as.numeric(data$TP))
}