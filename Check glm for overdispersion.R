#############################################################################################
##############################################################################################

#n Check glm for overdispersio. 
# Author: Daniel Gschwentner
# Date: 25/09/2020
# Modified from: http://r-eco-evo.blogspot.com/2017/05/generalized-linear-models.html#more
##############################################################################################
# Function.

check_overd <- function(GLM, p_threshold){
  # Overdispersion occurs when an important covariate has been omitted from the glm. 
  # For logistic models we can check for overdispersion by analysing the deviance residuals:
  # For logistic models the deviance residuals should follow a chi-square distribution with (n - p) degrees of freedom where p is the number of parameters estimated. 
  # Here we are checking for statistican significance. We want the p-value for the deviance residuals to be larger than the specified p-threshold. 
  
  
  p_value <- 1 - pchisq(GLM[["deviance"]], GLM[["df.residual"]])
  print(paste0("p-value = ", round(p_value, 3)))
  
  ratio <- GLM[["deviance"]]/GLM[["df.residual"]]
  print(paste0("GLM deviance/GLM df residual = ", round(ratio,2)))
  ifelse(ratio > 1.2, "GLM overdispersed; change parameters.", "GLM dispersion < 1.2. Overdispersion not an issue.")
  
  # Check the slope for statistical significance. 
  if(p_value > p_threshold){
    print("Model appropriate, checking for statistical significance of slope.")
    glm_anova <- anova(GLM, test = "Chisq")
    if(glm_anova[2,5] < p_threshold){
      print("Model slope is statistically significant.")
    }
    print(glm_anova)
      
  } else {print("Model overdisperesed, check parameters.")}

}

##############################################################################################
# Reproducible example. 

oak <- data.frame(survival = c(1,1,1,0,1,0,0,0,0,1,1, 1, 1,0,0,0,0,1,1),
                  area= c(5.41,5.63,25.92,15.17,13.04,18.85,33.95,22.87,
                          12.01,11.6,6.09,2.28,4.05,59.94,63.16,22.76,23.54,0.21,2.55))

oak_glm <- glm(survival~area, data=oak, family=binomial)

check_overd(GLM = oak_glm, p_threshold = 0.0005)



