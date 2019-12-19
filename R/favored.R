#' Classify teams that are favored to win
#'
#' Classify teams that are favored to win
#'
#' @param x An object from \code{\link{elo.run}} or \code{\link{elo.glm}}, or for the default method
#'   a vector representing wins.A.
#' @param p.A A vector of predicted win probabilities.
#' @param running logical, denoting whether to use the running predicted values.
#' @param ... Other arguments (not in use at this time).
#' @name favored.elo
NULL
#> NULL

#' @rdname favored.elo
#' @export
favored <- function(x, ...)
{
  UseMethod("favored")
}

#' @rdname favored.elo
#' @export
favored.elo.run <- function(x, ...)
{
  favored.default(x$elos[, sum(x$n.players) + 2], fitted(x))
}

truetiefalse <- function(x) factor(x, levels = c(1, 0.5, 0), labels = c("TRUE", "(tie)", "FALSE"))

#' @rdname favored.elo
#' @export
favored.elo.glm <- function(x, ...)
{
  if(x$outcome == "score") return(favored.default(x$y, x$fitted.values))
  table(truetiefalse(score(x$fitted.values, 0)),
        truetiefalse(score(x$y, 0)), dnn = c("Favored", "Actual"))
}

#' @rdname favored.elo
#' @export
favored.elo.running <- function(x, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  favored.default(x$y, x$running.values)
}

#' @rdname favored.elo
#' @export
favored.elo.markovchain <- favored.elo.glm

#' @rdname favored.elo
#' @export
favored.elo.winpct <- favored.elo.glm

#' @rdname favored.elo
#' @export
favored.elo.colley <- favored.elo.glm

#' @rdname favored.elo
#' @export
favored.default <- function(x, p.A, ...)
{
  table(truetiefalse(score(p.A, 0.5)), x, dnn = c("Favored", "Actual"))
}
