#############################################################################################
##############################################################################################

# Evaluate the performance of glms for simulated data. Only useful for glms using poisson distributions at the moment.  
# Author: Daniel Gschwentner
# Date: 25/09/2020
# Adopted from Smith and Warren, 2018 GLMs. Available from Nottingham Trent Uni: http://irep.ntu.ac.uk/id/eprint/37478/1/14596_Smith.pdf 

##############################################################################################
# Function.

sim.eval <- function(GLM){
  require(ggplot2)
  
  # Retrieve data used to build glm. 
  glm.data <- GLM[["data"]]
  # Create vector of unique repsonse variables. 
  unique.res <- unique(glm.data[,which(colnames(glm.data) == as.character(GLM[["formula"]][2]))])
  
  # Create simulated data using 10,000 observations. 
  n <- nrow(glm.data)
  fitted.vals <- fitted(GLM)
  response.sim <- matrix(nrow = n, ncol = 10000)
  vals.to.eval <-  vector(length = 10000)
  for(k in unique.res){
      for(i in 1:10000){
      response.sim[,i] <- rpois(n, lambda = fitted.vals) 
      vals.to.eval[i] <- sum(response.sim[, i] == k)/n }
    
    x <- as.data.frame(vals.to.eval)
    plot.obj <- ggplot(x, aes(x = vals.to.eval)) + 
      geom_histogram() + 
      labs(x = paste0("Percentage of ", k), y = "Frequency", subtitle = k) + 
      xlim(c(0.2, 0.6)) + 
      ylim(c(0, 1000)) + 
      scale_x_continuous(breaks = c(0.2, 0.3, 0.4, 0.5, 0.6), 
                                  labels = c("20%", "30%", "40%", "50%", "60%")) + 
      geom_point(aes(x = sum(unique.res == k) / n, y = 3),pch = 18, cex = 5, col = 1 ) + 
      theme_bw()
      plot(plot.obj)
    return(plot.obj)
    
  }
}


###########################################################################################
# Reproducible example. 
data("mtcars")

# Build glm. 
glm_mt <- glm(cyl ~ mpg + disp + am, data = mtcars, family = poisson)

sim.eval(glm_mt)  


