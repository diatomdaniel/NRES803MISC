#' QQ plot of a dataframe. 
#'
#' @param x a dataframe to plot.
#' @param trans a transformation to apply to the data. Passed as string. Currently working with "log10", "sqrt", "log", "log1p", etc. 
#' @param ... not used at present
#'
#' @return NULL called for its side effect, making a plot
#' @export
#'
#' @examples
#' qq_df(mtcars)
qq_df <- function(x, trans = NULL, ...){
  
  # Drop non-numerical data from dataframe. 
  numerics <- apply(x, 2, is.numeric)
  x <- x[,numerics]
  
  # Apply transformation if trans is not NULL. 
  if(!is.null(trans)) {
    x <- data.frame(apply(x, 2, trans))
  }
  
  # Create label for ylab. 
  if(!is.null(trans)) {
    ylabel <- paste0(trans, "(value)")
  } else {ylabel <- "value"}
  
  
  # Gather the data. 
  x <- tidyr::gather(x, key = "key", "value")
  
  # Plot the data. 
  fig <- ggplot2::ggplot(data = x, ggplot2::aes(sample = value)) + 
    ggplot2::stat_qq() + 
    ggplot2::stat_qq_line() + 
    ggplot2::facet_wrap(key~., scales = "free") + 
    ggplot2::labs(y =ylabel, x = "Theoretical quantiles") 
  
  return(fig)
  

}

  