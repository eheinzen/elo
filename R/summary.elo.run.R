
#' Summarize an \code{elo.run} Object
#'
#' @param object An object resulting from \code{\link{elo.run}}.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' summary(elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, k = 20))
#' @seealso \code{\link{elo.run}}, \code{\link{favored}}, \code{\link{auc.elo.run}}, \code{\link{mse}}
#' @name summary.elo.run
NULL
#> NULL

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
