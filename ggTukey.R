# Automated ggplot function for TukeyHSD. 
# Inputs are TukeyHSD model and sig. level. 
# If no input, sig. level assumed at 0.05. 

ggTukey <- function(Tukey, sig.level = NULL){

  # Some magic optional elements.
  if(is.null(sig.level)) {
    sig.level <- 0.05}
  
  # Create dataframe for 95% family-wise confidence level. 
  data <- data.frame(Tukey[[1]])
  data$"Stat.Sign" <- ifelse(data$p.adj < sig.level, "Stat. sign.", "Not stat. sign.")
  data$group <- rownames(data)  
  
  # GGplot. 
  f1 <- ggplot2::ggplot(data = data, 
                        ggplot2::aes(lty = `Stat.Sign`, group = group, 
                                     x = lwr, xend = upr, y = group, yend = group)) + 
    ggplot2::geom_segment() + 
    ggplot2::theme(legend.position = "bottom", 
                         legend.title = ggplot2::element_blank()) + 
    ggplot2::labs(x = "95% family-wise confidence intervall", y = NULL)
  
  return(f1)
}

# Example. 
#data("mtcars")
#aov.mtcars <- aov(disp ~ factor(gear), data = mtcars)
#ggTukey(TukeyHSD(aov.mtcars), sig.level = 0.01)
