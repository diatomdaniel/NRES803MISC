# Function to create tabluar summaries of numerical data similar to summary but in format of a data frame. 

rubbish_summary <- function(dataframe, dec_point){
  # Select variables that are numeric. Drops all factors or character variables from dataset.
  numerics <- unlist(lapply(dataframe[,1:ncol(dataframe)], is.numeric))
  numerics <- dataframe[,numerics]

  # Get summary for numeric data. 
  # Gather. 
  numerics_out <- tidyr::gather(numerics, "Variable", "Value")
  # Grouping variables. 
  numerics_out <- dplyr::group_by(numerics_out, Variable)
  # Summarise. 
  num_summary <- dplyr::summarise(numerics_out, 
                                  "Abundance" = n()-sum(is.na(Value)), 
                                  "Min" = min(Value, na.rm = T), 
                                  "Median" = median(Value, na.rm = T), 
                                  "Mean" = mean(Value, na.rm = T), 
                                  "Max" = max(Value, na.rm = T), 
                                  "St.Dev" = sd(Value, na.rm = T))
  
  # Round to specified decimal places. 
  if(missing(dec_point)){
    num_summary[,-c(1:2)] <- lapply(num_summary[,-c(1:2)], round, digits = 3)
  } else {
    num_summary[,-c(1:2)] <- lapply(num_summary[,-c(1:2)], round, digits = dec_point)
  }
  
  # Return object. 
  return(num_summary)
  
}

# Example. 
#data("mtcars")
#test <- rubbish_summary(mtcars)
# Or. 
#test <- rubbish_summary(mtcars, dec_point = 5)
