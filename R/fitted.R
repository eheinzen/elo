
#' Extract model values
#'
#' Extract model values from \code{elo} functions.
#'
#' @param object An object.
#' @param ... Other arguments
#' @param running logical, denoting whether to use the running predicted values.
#' @return A vector of fitted values. For running values, it has an additional attribute denoting to which
#'   group (i.e., which model) the prediction belongs
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
  a <- stats::napredict(object$na.action, attr(object$running.values, "group"))
  out <- stats::napredict(object$na.action, object$running.values)
  attr(out, "group") <- a
  out
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
