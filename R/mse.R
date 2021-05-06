#' Calculate the mean square error
#'
#' Calculate the mean square error (Brier score) for a model.
#' @param object An object
#' @param subset (optional) A vector of indices on which to calculate
#' @param running logical, denoting whether to use the running predicted values.
#' @param discard.skipped Logical, denoting whether to ignore the skipped observations in the calculation
#' @param ... Other arguments (not used at this time).
#' @details Even though logistic regressions don't use the MSE on the y=0/1 scale, it can still be informative.
#'   Note that the S3 method is \code{mse}.
#' @name elo.mse
NULL
#> NULL

#' @rdname elo.mse
#' @export
mse <- function(object, ..., subset = TRUE)
{
  UseMethod("mse")
}

#' @rdname elo.mse
#' @export
brier <- mse

#' @rdname elo.mse
#' @export
mse.elo.run <- function(object, ..., subset = TRUE)
{
  r <- residuals(object)
  mean(r[subset]^2)
}

#' @rdname elo.mse
#' @export
mse.elo.glm <- function(object, ..., subset = TRUE)
{
  r <- object$fitted.values - object$y
  mean(r[subset]^2)
}

#' @rdname elo.mse
#' @export
mse.elo.running <- function(object, running = TRUE, discard.skipped = FALSE, ..., subset = TRUE)
{
  if(!running) return(NextMethod())
  if(!is.logical(subset)) stop("'subset' must be logical for this functionality")
  idx <- attr(object$running.values, "group") > (if(discard.skipped) 0 else -1)
  r <- object$running.values - object$y
  mean(r[idx & subset]^2)
}

#' @rdname elo.mse
#' @export
mse.elo.markovchain <- mse.elo.glm

#' @rdname elo.mse
#' @export
mse.elo.winpct <- mse.elo.glm

#' @rdname elo.mse
#' @export
mse.elo.colley <- mse.elo.glm
