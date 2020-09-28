model_eval <- function(df){
# Model to return summary plots of fitted models. Input is a data frame of the augmented model. 
  # I.e. df = augment(lm, data = data). 
  # Four summary plots that are arranged in ggppubr. 
  
  require(tidyverse)
  require(ggpubr)
  
  # Resid ~ Fitted.
  p1 <- df %>% ggplot(aes(.fitted, .resid)) + 
    geom_point() + geom_smooth() + 
    labs(x = "Fitted values",y = "Residual")
  
  # Sample vs theoretical quantiles. 
  y <- quantile(df$.resid, c(0.25,0.75))
  x <- qnorm(c(0.25,0.75))
  sl = diff(y)/diff(x)
  int <- y[1L] - sl * x[1L]
  
  # Create plot 2. 
  p2 <- ggplot(df, aes(sample = .resid)) + 
    stat_qq() + 
    geom_abline(aes(intercept = int, slope = sl), col = "red") + 
    labs(x = "Theoretical quantiles", y = "Sample quantiles")
  
  # Sqrt Resid.   
  p3 <- ggplot(df, aes(.fitted, sqrt(abs(.resid)))) + 
    geom_point() + geom_smooth() + 
    geom_hline(yintercept = 1) + 
    labs(x = "Fitted values", y = "Standardised residuals")
  
  p4 <- ggplot(df, aes(.hat, .std.resid)) +
    geom_vline(size = 1, colour = "red", xintercept = 0) +
    geom_hline(size = 1, colour = "red", yintercept = 0) +
    geom_point(aes(size = .cooksd)) + geom_smooth(se = FALSE) + 
    labs(x = "Leverage", y = "Standardised residuals")
  

  
  p6 <- ggarrange(p1,p2,p3,p4, labels = c("a", "b", "c", "d"))
 
  
   return(p6)
  

  
}




