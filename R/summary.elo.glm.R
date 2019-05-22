
#' Summarize an \code{elo.glm} Object
#'
#' @param object An object resulting from \code{\link{elo.glm}}.
#' @param ... Other arguments
#' @param running logical, denoting whether to use the running predicted values.
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
fitted.elo.glm.running <- function(object, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  stats::napredict(object$na.action, object$running.values)
}

#' @rdname summary.elo.glm
#' @export
summary.elo.glm <- function(object, ...)
{
  out <- NextMethod()
  out$favored <- favored(object, ...)
  out$mse <- mse(object, ...)
  out$auc <- auc.elo.glm(object, ...)
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
