#' Calculate the mean square error
#'
#' Calculate the mean square error (Brier score) for a model.
#' @param object An object
#' @param subset (optional) A vector of indices on which to calculate the MSE.
#' @param running logical, denoting whether to use the running fitted values.
#' @param ... Other arguments (not in use at this time).
#' @details Even though logistic regressions don't use the MSE on the y=0/1 scale, it can still be informative.
#'   Note that the S3 method is \code{mse}.
#' @name elo.mse
NULL
#> NULL

#' @rdname elo.mse
#' @export
brier <- function(object, subset, ...)
{
  UseMethod("mse")
}

#' @rdname elo.mse
#' @export
mse <- function(object, subset, ...)
{
  UseMethod("mse")
}

#' @rdname elo.mse
#' @export
mse.elo.run <- function(object, subset, ...)
{
  r <- residuals(object)
  if(!missing(subset)) r <- r[subset]
  mean(r^2)
}

#' @rdname elo.mse
#' @export
mse.elo.glm <- function(object, subset, ...)
{
  r <- object$fitted.values - object$y
  if(!missing(subset)) r <- r[subset]
  mean(r^2)
}

#' @rdname elo.mse
#' @export
mse.elo.glm.running <- function(object, subset, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  r <- object$running.fitted.values - object$y
  if(!missing(subset)) r <- r[subset]
  mean(r^2)
}