if(getRversion() >= "2.15.1")  utils::globalVariables(c(".fitted", ".resid", ".std.resid", ".hat", ".cooksd"))

#' Plot residuals of a fitted model
#'
#' This function replicates plot.lm() functionality with ggplot
#'
#' @param x a fitted model object that has broom tidiers
#' @param ... not used at present
#'
#' @return NULL called for its side effect, making a plot
#' @export
#'
#' @examples
#' lm1 <- lm(Sepal.Width ~ Petal.Length, data = iris)
#' check_assumptions(lm1)
check_assumptions <- function(x, ...){
 
  my_theme <- ggplot2::theme_classic() +
    ggplot2::theme(text=ggplot2::element_text(size=ggplot2::rel(4)))

  # get the residuals etc.
  rr <- broom::augment(x)
  ## plot 0 resid vs. fitted
  rr0 <- ggplot2::ggplot(rr, ggplot2::aes(x = .fitted, y = .std.resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", formula = y~x) +
    ggplot2::geom_hline(yintercept = 0, linetype=2) +
    my_theme +
    ggplot2::labs(x = "Fitted values", y = "Standardised residuals")
  ## plot 1 qq plot
  # get int and slope for qqline
  probs <- c(0.25,0.75)
  y <- stats::quantile(rr$.std.resid, probs, names = FALSE, na.rm = TRUE)
  x <- stats::qnorm(probs)
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  rr1 <- ggplot2::ggplot(rr, ggplot2::aes(sample=.std.resid)) +
    ggplot2::geom_abline(intercept = int, slope = slope, linetype = 2, size = 2, col = "red") +
    ggplot2::geom_qq(size=ggplot2::rel(4)) +
    my_theme + 
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles")

  ## plot 2 scale location plot
  rr2 <- ggplot2::ggplot(rr, ggplot2::aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", formula = y~x) +
    ggplot2::geom_hline(yintercept  = 0.822179) +
    my_theme + 
    ggplot2::labs(x = "Fitted values", y = expression(sqrt("Standardised residuals")))
  
  ## plot 3 cooks distance plot
  rr3 <- ggplot2::ggplot(rr, ggplot2::aes(.hat, .std.resid)) +
    ggplot2::geom_vline(size = 2, xintercept = 0) +
    ggplot2::geom_hline(size = 2, yintercept = 0) +
    ggplot2::geom_point(ggplot2::aes(size = .cooksd)) +
    ggplot2::geom_smooth(method="loess", formula = y~x, se = FALSE) +
    my_theme + 
    ggplot2::labs(x = "Leverage (Hat-matrix)", y = "Standardised residuals")

  ggpubr::ggarrange(rr0, rr1, rr2, rr3, nrow=2, ncol = 2)

  

}
