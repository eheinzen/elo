
#' Summarize an \code{elo.colley} Object
#'
#' @param object An object resulting from \code{\link{elo.colley}}.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' co <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, subset = points.Home != points.Visitor)
#' summary(co)
#' @seealso \code{\link{elo.colley}}, \code{\link{favored}}, \code{\link{auc.elo.colley}}, \code{\link{mse}}
#' @name summary.elo.colley
NULL
#> NULL

#' @rdname summary.elo.colley
#' @export
summary.elo.colley <- function(object, ...)
{
  object$favored <- favored(object, ...)
  object$mse <- mse(object, ...)
  object$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.colley(object, ...)
  class(object) <- c("summary.elo.colley", class(object))
  object
}

#' @export
print.summary.elo.colley <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}
