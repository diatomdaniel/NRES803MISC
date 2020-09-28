##############################################################################################
##############################################################################################

# Confidence Intervals for GLMs. 
# Author: Daniel Gschwentner
# Date: 25/09/2020
# Modified from: https://fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/

##############################################################################################
# Function.
glm_conf <- function(GLM){
  
  # Packages.
  require(tidyverse)
  
  # Get names of variables used to build glm. 
  GLM_data <- GLM$data
  GLM
  
  x.y <- names(GLM_data)
  
  # Bind the predictor and predicted variables into a dataframe. 
  df <- data.frame(Independent.var = GLM$data[,1], 
                   Dependent.var = GLM$data[,2])
  # Assign the new columns the "old" names
  colnames(df) <- x.y
  
  
  # Get link function. 
  link <- family(GLM)$linkinv
  
  # Create data to calculate confidence intervals. 
  data_conf <- with(df, 
                    data.frame(X = seq(
                      min(df[,1]), max(df[,1]), length = 100)))
  
  # Rename the column with the "old" name of the independent variable.
  colnames(data_conf) <- x.y[1]
  
  # Calculate confidence intervals.
  data_conf <- cbind(data_conf, 
                     predict(GLM, data_conf, 
                             type = "link",se.fit = T)[1:2])
  
  data_conf <- transform(data_conf, 
                         Fitted.vals = link(fit), 
                         Upper.95.conf = link(fit + (2*se.fit)), 
                         Lower.05.conf = link(fit - (2*se.fit)))
  
  # Create a ggplot to visualise the results. 
  conf_plot <- ggplot(data_conf, aes(data_conf[,1], data_conf[,4])) + 
    geom_ribbon(data = data_conf, aes(ymin = Lower.05.conf, 
                                      ymax = Upper.95.conf, 
                                      x = data_conf[,1]), 
                fill = "steelblue2", alpha = 0.2, inherit.aes = FALSE) + 
    geom_line(data = data_conf, aes(x = data_conf[,1], y = Fitted.vals)) + 
    geom_point(data = df, aes(df[,1], as.numeric(df[,2]))) + 
    labs(x = x.y[1], y = paste0("Probability of ", x.y[2]))
  
  # Return list with confidence intervals and ggplot object. 
  out <- list(Confidence.Limits = data_conf, Plot = conf_plot)
  
  
  
  
}

##############################################################################################
# Reproducible example. 
# Mtcars data. 
data("mtcars") 


# Transform am into binomial variable. 
mtcars$logical_am <- as.logical(mtcars$am)

# Annoyingly we need to reduce the mtcars data frame to the two variables we will use to build the glm. 
# I haven't figured out how to use the functions with more than two variables or how to get the column names efficiently from the glm-object. Any ideas?
mtcars <- mtcars %>% select(mpg, logical_am)

# Create glm with mpg. I don't know if this makes logical sense but it's just an example. 
mtcars_glm <- glm(logical_am ~ mpg, data = mtcars, family = binomial)
summary(mtcars_glm)

# Apply the new function. 
glm_conf(mtcars_glm)




##############################################################################################
##############################################################################################
