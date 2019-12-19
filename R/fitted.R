
#' Extract model values
#'
#' Extract model values from \code{elo} functions.
#'
#' @param object An object.
#' @param ... Other arguments
#' @param running logical, denoting whether to use the running predicted values.
#' @name fitted.elo
NULL
#> NULL

## stats:::terms.default and stats:::weights.default also work

#' @rdname fitted.elo
#' @export
fitted.elo.run <- function(object, ...)
{
  out <- object$elos[, sum(object$n.players) + 1]
  stats::napredict(object$na.action, out)
}

#' @rdname fitted.elo
#' @export
residuals.elo.run <- function(object, ...)
{
  out <- object$elos[, sum(object$n.players) + 2] - object$elos[, sum(object$n.players) + 1]
  stats::naresid(object$na.action, out)
}

#' @rdname fitted.elo
#' @export
fitted.elo.running <- function(object, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  stats::napredict(object$na.action, object$running.values)
}

#' @rdname fitted.elo
#' @export
fitted.elo.glm <- function(object, ...)
{
  stats::napredict(object$na.action, object$fitted.values)
}

#' @rdname fitted.elo
#' @export
fitted.elo.markovchain <- fitted.elo.glm

#' @rdname fitted.elo
#' @export
fitted.elo.winpct <- fitted.elo.glm

#' @rdname fitted.elo
#' @export
fitted.elo.colley <- fitted.elo.glm
