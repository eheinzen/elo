
#' Extract model values
#'
#' Extract model values from \code{elo} functions.
#'
#' @param object An object.
#' @param ... Other arguments
#' @param running logical, denoting whether to use the running predicted values.
#' @name elo.fitted
NULL
#> NULL

## stats:::terms.default and stats:::weights.default also work

#' @rdname elo.fitted
#' @export
fitted.elo.run <- function(object, ...)
{
  out <- object$elos[, sum(object$n.players) + 1]
  stats::napredict(object$na.action, out)
}

#' @rdname elo.fitted
#' @export
residuals.elo.run <- function(object, ...)
{
  out <- object$elos[, sum(object$n.players) + 2] - object$elos[, sum(object$n.players) + 1]
  stats::naresid(object$na.action, out)
}

#' @rdname elo.fitted
#' @export
fitted.elo.running <- function(object, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  stats::napredict(object$na.action, object$running.values)
}

#' @rdname elo.fitted
#' @export
fitted.elo.glm <- function(object, ...)
{
  stats::napredict(object$na.action, object$fitted.values)
}

#' @rdname elo.fitted
#' @export
fitted.elo.markovchain <- fitted.elo.glm

#' @rdname elo.fitted
#' @export
fitted.elo.winpct <- fitted.elo.glm
