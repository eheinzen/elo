
#' Summarize an \code{elo.run} Object
#'
#' @param object An object resulting from \code{\link{elo.run}}.
#' @param subset A vector of indices on which to calculate the MSE.
#' @param x An object of class \code{"summary.elo.run"}.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' summary(elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, k = 20))
#' @name summary.elo.run
NULL
#> NULL


#' @rdname summary.elo.run
#' @export
favored <- function(x, ...)
{
  table(factor(score(fitted(x), 0.5), levels = c(1, 0.5, 0), labels = c("TRUE", "(tie)", "FALSE")),
        x$elos[, sum(x$n.players) + 2], dnn = c("Favored", "Actual"))
}

#' @rdname summary.elo.run
#' @export
fitted.elo.run <- function(object, ...)
{
  object$elos[, sum(object$n.players) + 1]
}

#' @rdname summary.elo.run
#' @export
residuals.elo.run <- function(object, ...)
{
  object$elos[, sum(object$n.players) + 2] - fitted(object)
}

#' @rdname summary.elo.run
#' @export
mse <- function(object, subset)
{
  sq <- if(missing(subset)) residuals(object)^2 else (residuals(object)[subset])^2
  mean(sq)
}

#' @rdname summary.elo.run
#' @export
summary.elo.run <- function(object, ...)
{
  object$favored <- favored(object)
  object$mse <- mse(object)
  object$auc <- auc.elo.run(object)
  class(object) <- c("summary.elo.run", class(object))
  object
}

#' @rdname summary.elo.run
#' @export
print.summary.elo.run <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}
