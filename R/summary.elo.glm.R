
#' Summarize an \code{elo.glm} Object
#'
#' @param object An object resulting from \code{\link{elo.glm}}.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' summary(elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament))
#' @seealso \code{\link{elo.glm}}, \code{\link{favored}}, \code{\link{auc.elo.run}}, \code{\link{mse}}
#' @name summary.elo.glm
NULL
#> NULL

#' @rdname summary.elo.glm
#' @export
summary.elo.glm <- function(object, ...)
{
  out <- NextMethod()
  out$favored <- favored(object, ...)
  out$mse <- mse(object, ...)
  out$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.glm(object, ...)
  class(out) <- c("summary.elo.glm", class(out))
  out
}

#' @export
print.summary.elo.glm <- function(x, ...)
{
  NextMethod()
  cat("Mean Square Error: ", round(x$mse, 4), "\n",
      "AUC: ", round(x$auc, 4), "\n",
      "Favored Teams vs. Actual Wins: \n",
      sep = "")
  print(x$favored)

  invisible(x)
}
