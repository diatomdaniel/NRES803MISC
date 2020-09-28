#############################################################################################
##############################################################################################

# Estimate possible model configurations for glms. 
# Author: Daniel Gschwentner
# Date: 25/09/2020

##############################################################################################
# Function.

get_models_glm <- function(response, predictors, FUN, family, dataset){
  require(tidyverse)
  
  # Empty list for storing variables. 
  models <- list()
  
  # Create vector of operators. 
  ops <- c("+", "-", "/", "*")
  
  # Create loop to iterate over predictor names. 
  for(i in 1:length(predictors)){
    vc <- combn(predictors, i)
    for(j in 1:ncol(vc)){
        model <- as.formula(paste0(response, " ~ ", paste0(vc[,j], collapse = sample(ops, replace = T))))
        models <- c(models, model)
    }
  }
  
  # Map the models to the data. 
  fits <- purrr::map(models, FUN, family = family, dataset)
  
  # Create output list. 
  out <- list(models = models, model.fits = fits)
  return(out)
}

##############################################################################################
# Reproducible example. 
# Mtcars data. 
data("mtcars")

models <- get_models(colnames(mtcars)[9], colnames(mtcars)[-c(1,9)], glm, binomial, mtcars)
lapply(models$model.fits, summary)


