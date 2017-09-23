
#' Helper functions for \code{elo.run}
#'
#' \code{as.matrix.elo.run} converts an object from \code{\link{elo.run}} into a matrix of running Elos.
#'
#' \code{as.data.frame.elo.run} converts the \code{"elos"} component of an object from \code{\link{elo.run}} into a data.frame.
#'
#' \code{last} is a generic function, whose \code{\link{elo.run}} method extracts the last Elo per team.
#'
#' @param x An object of class \code{"elo.run"}.
#' @param ... Other arguments (Not in use at this time).
#' @return A matrix, a data.frame, or a named vector.
#' @examples
#' e <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'              data = tournament, k = 20)
#' head(as.matrix(e))
#' str(as.data.frame(e))
#' final.elos(e)
#' @seealso \code{\link{elo.run}}
#' @name elo.run.helpers
NULL
#> NULL

#' @rdname elo.run.helpers
#' @export
as.matrix.elo.run <- function(x, ...)
{
  stopifnot(length(x$teams) == ncol(x$elos.regressed))
  out <- eloRunAsMatrix(x$elos, x$elos.regressed[1, ])
  colnames(out) <- x$teams
  out
}


#' @rdname elo.run.helpers
#' @export
as.data.frame.elo.run <- function(x, ...)
{
  out <- as.data.frame(x$elos)
  out$team <- factor(out$team, levels = seq_along(x$teams), labels = x$teams)
  out
}

#' @rdname elo.run.helpers
#' @export
final.elos <- function(x, ...)
{
  UseMethod("final.elos")
}

#' @rdname elo.run.helpers
#' @export
final.elos.elo.run <- function(x, ...)
{
  y <- as.data.frame(x, ...)
  idx <- !duplicated(y$team, fromLast = TRUE)
  tmp <- y$elo[idx]
  names(tmp) <- as.character(y$team[idx])
  tmp[match(names(tmp), x$teams)]
}


