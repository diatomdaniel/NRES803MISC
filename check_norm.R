# Script to test for normality of data using histograms, qqplots and 
# Shapiro-Wilk-test. 


check_norm <- function(variable, type = "hist",  binwidth = NULL){
  # Transform data variable to dataframe for plotting and easy handling. 
  variable <- variable[!is.na(variable)]
  variable <- as.data.frame(variable)
  
  # Some magic optional elements.
  if(is.null(binwidth)) {
    binwidth <- 30}
  
  # Histogram or density plot of values. 
  if(type == "hist") {
    f1 <- ggplot2::ggplot(data = variable, ggplot2::aes(variable)) + 
      ggplot2::geom_histogram(fill = "white", col = "black", binwidth = binwidth) +
      ggplot2::theme_bw() + 
      ggplot2::labs(y = "Count", x = "Variable") } else {
    f1 <- ggplot2::ggplot(data = variable, ggplot2::aes(variable)) + 
      ggplot2::geom_density(fill = "blue", col = "black", alpha = .3) + 
      ggplot2::theme_bw() + 
      ggplot2::labs(y = "Density", x = "Variable")}
  
  # QQPlot of values. 
  # Calculate normal/theoretical quantiles. 
  x <- qnorm(c(0.25, 0.75))
  y <- quantile(variable$variable, c(0.25, 0.75))
  sl <- diff(y)/diff(x)
  int <- y[1L] -sl *x [1L]
  
  f2 <- ggplot2::ggplot(data = variable, ggplot2::aes(sample = variable)) + 
    ggplot2::stat_qq()+ 
    ggplot2::geom_abline(slope = sl, intercept = int) +
    ggplot2::theme_bw() + 
    ggplot2::labs(y = "Sample quantiles", x = "Theoretical quantiles")
  
  # Arrange the plots. 
  f3 <- ggpubr::ggarrange(f1, f2, ncol= 2, nrow = 1)
  
  # Calculate Shaprio-Wilks test to check for nomality. 
  sw_test <- shapiro.test(variable$variable)
  
  # Print message dependent on p-value.
  if(sw_test[["p.value"]] < 0.05) {
    print("Shapiro-Wilk test: P-Value < 0.05 signifcance level; reject assumption of normality.")
  } else {
    print("Shapiro-Wilk test: P-Value > 0.05 signifcance level; Assumption of normality is valid.")}
  
  print(f3)
  
}

# Examples. 
check_norm(mtcars$disp, type = "hist", binwidth = 50)
check_norm(mtcars$disp)
check_norm(mtcars$disp, type = "density")
