
#' Summarize an \code{elo.winpct} Object
#'
#' @param object An object resulting from \code{\link{elo.winpct}}.
#' @param ... Other arguments
#' @return A summary of \code{object}.
#' @examples
#' wp <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'   data = tournament, subset = points.Home != points.Visitor, k = 0.7)
#' summary(wp)
#' @seealso \code{\link{elo.winpct}}, \code{\link{favored}}, \code{\link{auc.elo.winpct}}, \code{\link{mse}}
#' @name summary.elo.winpct
NULL
#> NULL

#' @rdname summary.elo.winpct
#' @export
summary.elo.winpct <- function(object, ...)
{
  object$favored <- favored(object, ...)
  object$mse <- mse(object, ...)
  object$auc <- if(object$outcome == "mov") NA_real_ else auc.elo.winpct(object, ...)
  class(object) <- c("summary.elo.winpct", class(object))
  object
}

#' @export
print.summary.elo.winpct <- print.summary.elo.markovchain
