#############################################################################################
##############################################################################################

# Check the Pearson Residuals (Standardised residuals) for non-linear behaviour. 
# Author: Daniel Gschwentner
# Date: 25/09/2020

##############################################################################################

check.glm.nonlinear <- function(GLM){
  require(tidyverse)
  
  # Retrieve the residuals from the GLM. 
  pear.resid <- residuals.glm(GLM, type = "pearson")
  
  # Retrieve the data used to build the GLM. We want to plot the residuals against them. 
  glm.data <- GLM[["data"]] # I do not know how to limit this to only the columns used to build the models. 
  
  # Add residuals. 
  glm.data$pear.residuals <- pear.resid
  
  # Gather values. 
  glm.data2 <- glm.data %>% gather("key", "value", -pear.residuals)
  
  # Plot the results. 
  glm.data2 %>% ggplot(aes(value, pear.residuals)) + 
    geom_point() + 
    geom_smooth(method = "loess", col = "black", alpha = 0) + 
    facet_wrap(key~., scales = "free") + 
    labs(y = "Pearson residuals", x = NULL) + theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = "black"))

}


###########################################################################################
# Reproducible example. 
data("mtcars")

# Build glm. 
glm_mt <- glm(cyl ~ mpg + disp + am, data = mtcars, family = poisson)

check.glm.nonlinear(glm_mt)  
