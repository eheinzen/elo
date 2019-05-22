#' Classify teams that are favored to win
#'
#' Classify teams that are favored to win
#'
#' @param x An object from \code{\link{elo.run}} or \code{\link{elo.glm}}, or for the default method
#'   a vector representing wins.A.
#' @param p.A A vector of predicted win probabilities.
#' @param running logical, denoting whether to use the running predicted values.
#' @param ... Other arguments (not in use at this time).
#' @name elo.favored
NULL
#> NULL

#' @rdname elo.favored
#' @export
favored <- function(x, ...)
{
  UseMethod("favored")
}

#' @rdname elo.favored
#' @export
favored.elo.run <- function(x, ...)
{
  favored.default(x$elos[, sum(x$n.players) + 2], fitted(x))
}

#' @rdname elo.favored
#' @export
favored.elo.glm <- function(x, ...)
{
  favored.default(x$y, x$fitted.values)
}

#' @rdname elo.favored
#' @export
favored.elo.glm.running <- function(x, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  favored.default(x$y, x$running.values)
}

#' @rdname elo.favored
#' @export
favored.default <- function(x, p.A, ...)
{
  table(factor(score(p.A, 0.5), levels = c(1, 0.5, 0), labels = c("TRUE", "(tie)", "FALSE")),
        x, dnn = c("Favored", "Actual"))
}
