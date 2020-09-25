#############################################################################################
##############################################################################################

# Estimate possible model configurations. 
# Author: Daniel Gschwentner
# Date: 25/09/2020

##############################################################################################
# Function.

get_models <- function(response, predictors, FUN, dataset){
  
  # Empty list for storing variables. 
  models <- list()
  
  # Create loop to iterate over predictor names. 
  for(i in 1:length(predictors)){
    vc <- combn(predictors, i)
    for(j in 1:ncol(vc)){
      for(k in c("+", "-", "/", "*")){
        model <- as.formula(paste0(response, " ~ ", paste0(vc[,j], collapse = k)))
        models <- c(models, model)
      }
    }
  }
  
  # Map the models to the data. 
  fits <- map(models, FUN, dataset)
  
  # Create output list. 
  out <- list(models = models, model.fits = fits)
  return(out)
}

##############################################################################################
# Reproducible example. 
# Mtcars data. 
data("mtcars")

models <- get_models(colnames(mtcars)[1], colnames(mtcars)[-1], lm, mtcars)
lapply(models$model.fits, summary)
