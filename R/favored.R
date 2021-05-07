#' Classify teams that are favored to win
#'
#' Classify teams that are favored to win
#'
#' @param x An object from \code{\link{elo.run}} or \code{\link{elo.glm}}, or for the default method
#'   a vector representing wins.A.
#' @param p.A A vector of predicted win probabilities.
#' @inheritParams mse
#' @name favored.elo
NULL
#> NULL

#' @rdname favored.elo
#' @export
favored <- function(x, ..., subset = TRUE)
{
  UseMethod("favored")
}

#' @rdname favored.elo
#' @export
favored.elo.run <- function(x, ..., subset = TRUE)
{
  favored.default(x$elos[subset, sum(x$n.players) + 2], fitted(x)[subset])
}

truetiefalse <- function(x) factor(x, levels = c(1, 0.5, 0), labels = c("TRUE", "(tie)", "FALSE"))

#' @rdname favored.elo
#' @export
favored.elo.glm <- function(x, ..., subset = TRUE)
{
  if(x$outcome == "score") return(favored.default(x$y[subset], x$fitted.values[subset]))
  table(truetiefalse(score(x$fitted.values[subset], 0)),
        truetiefalse(score(x$y[subset], 0)), dnn = c("Favored", "Actual"))
}

#' @rdname favored.elo
#' @export
favored.elo.running <- function(x, running = TRUE, discard.skipped = FALSE, ..., subset = TRUE)
{
  if(!running) return(NextMethod())
  if(!is.logical(subset)) stop("'subset' must be logical for this functionality")
  idx <- attr(x$running.values, "group") > (if(discard.skipped) 0 else -1)
  favored.default(x$y[idx & subset], x$running.values[idx & subset])
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
