#' List method for An Information Criterion
#'
#' @param object list of fitted model objects for which logLik method exists
#' @param ... ignored with a warning
#' @param k numeric, the penalty per parameter to be used; default k = 2 is the classical AIC
#'
#' @return A `data.frame` with rows corresponding to the members of object,
#' and columns for the number of parameters in the model and the IC.
#'
#' @export
#'
AIC.list <- function (object, ..., k = 2)
{
  ll <- if (isNamespaceLoaded("stats4"))
    stats4::logLik
  else stats::logLik
  if (!missing(...)) {
    warning("... not empty, only first argument used.")
  }
  lls <- lapply(object, ll)
  vals <- sapply(lls, function(el) {
    no <- attr(el, "nobs")
    c(as.numeric(el), attr(el, "df"), if (is.null(no)) NA_integer_ else no)
  })
  val <- data.frame(df = vals[2L, ], ll = vals[1L, ])
  nos <- stats::na.omit(vals[3L, ])
  if (length(nos) && any(nos != nos[1L]))
    warning("models are not all fitted to the same number of observations")
  val <- data.frame(df = val$df, AIC = -2 * val$ll + k *
                      val$df)
  if (!is.null(names(object))){
    row.names(val) <- names(object)
  } else {
    row.names(val) <- 1L:length(object)
  }
  val

}
