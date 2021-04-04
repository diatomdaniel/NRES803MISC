#' Plot model coefficients as segments. 
#' Model coefficients are plotted as the interquartile range of the independent variables times the regression coefficient.
#'
#' @param model a fitted model object 
#' @param ... not used at present
#'
#' @return NULL called for its side effect, making a plot
#' @export
#'
#' @examples
#' lm1 <- lm(Sepal.Width ~ Petal.Length, data = iris)
#' mod_plot_seg(lm1)


mod_plot_seg <- function(model, ...){
  # Grab coefs of model. 
  modcoef <- data.frame(coef(summary(model)))
  # Colnames. 
  colnames(modcoef) <- c("Estimate", "St.Error", "t_value", "P_value")
  # Compute IQR of independent variables x regression coeffs. 
  mod_data <- model.frame(model)
  iqr <- lapply(mod_data[,2:ncol(mod_data)], IQR)
  iqr <- unlist(iqr)
  for(i in 2:nrow(modcoef)){
    modcoef[i,"IQRxreg"] <- modcoef[i,1] * iqr[i-1]
  }
  # Drop first row with intercept. 
  modcoef <- modcoef[-1,]
  # Add column with names. 
  modcoef$names <- rownames(modcoef)
  # Sig col. 
  modcoef$sig <- ifelse(modcoef$P_value < 0.05, "Significant", "Not significant")
  
  # Plot. 
  f1 <- ggplot2::ggplot(data = modcoef, aes(y = names, lty = sig)) +
    ggplot2::geom_segment(aes(yend = names, x = 0, xend = IQRxreg), col = "black") + 
    ggplot2::geom_point(aes(x = IQRxreg)) +
    ggplot2::geom_vline(xintercept = 0) + 
    ggplot2::scale_y_discrete(limits=rev) + 
    ggplot2::labs(x = "Interquartile effect size", y = NULL)  + 
    ggplot2::scale_linetype_manual(values = c("Significant" = 1, 
                                              "Not significant" = 2), 
                                   name = NULL ) + 
    ggplot2::theme(legend.position = "bottom")
  
  return(f1)
    
}

