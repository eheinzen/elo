
#' Helper functions for \code{elo.run}
#'
#' \code{as.matrix.elo.run} converts an object from \code{\link{elo.run}} into a matrix of running Elos.
#'
#' \code{as.data.frame.elo.run} converts the \code{"elos"} componenet of an object from \code{\link{elo.run}} into a data.frame.
#'
#' \code{last} is a generic function, whose \code{\link{elo.run}} method extracts the last Elo per team.
#'
#' @param x An object of class \code{"elo.run"}.
#' @param ... Other arguments (Not in use at this time).
#' @return A matrix, a data.frame, or a named vector.
#'
#' @seealso \code{\link{elo.run}}
#' @name elo.run.helpers
NULL
#> NULL

#' @rdname elo.run.helpers
#' @export
as.matrix.elo.run <- function(x, ...)
{
  out <- eloRunAsMatrix(x$elos)
  colnames(out) <- x$teams
  out
}


#' @rdname elo.run.helpers
#' @export
as.data.frame.elo.run <- function(x, ...)
{
  out <- as.data.frame(x$elos)
  out$Team <- factor(out$Team, levels = seq_along(x$teams) - 1, labels = x$teams)
  out
}

#' @rdname elo.run.helpers
#' @export
last <- function(x, ...)
{
  UseMethod("last")
}

#' @rdname elo.run.helpers
#' @export
last.elo.run <- function(x, ...)
{
  y <- as.data.frame(x, ...)
  idx <- !duplicated(y$Team, fromLast = TRUE)
  tmp <- setNames(y$Elo[idx], as.character(y$Team[idx]))
  tmp[match(names(tmp), x$teams)]
}


