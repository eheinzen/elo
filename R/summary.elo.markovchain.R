
#' Summarize an \code{elo.markovchain} Object
#'
#' @param object An object resulting from \code{\link{elo.markovchain}}.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, subset = points.Home != points.Visitor, k = 0.7)
#' summary(mc)
#' @seealso \code{\link{elo.markovchain}}, \code{\link{favored}}, \code{\link{auc.elo.markovchain}}, \code{\link{mse}}
#' @name summary.elo.markovchain
NULL
#> NULL

#' @rdname summary.elo.markovchain
#' @export
summary.elo.markovchain <- function(object, ...)
{
  object$favored <- favored(object, ...)
  object$mse <- mse(object, ...)
  object$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.markovchain(object, ...)
  class(object) <- c("summary.elo.markovchain", class(object))
  object
}

#' @export
print.summary.elo.markovchain <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}
