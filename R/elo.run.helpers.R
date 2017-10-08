
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
#' @param group A grouping vector, telling which rows to output in the matrix.
#' @return A matrix, a data.frame, or a named vector.
#' @examples
#' e <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week),
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
as.matrix.elo.run <- function(x, ..., group = x$group)
{
  check_as_matrix(x, group)

  out <- eloRunAsMatrix(x$elos, x$initial.elos, x$elos.regressed,
                        check_group_regress(x$regress),
                        check_group_regress(group, gt.zero = TRUE))
  colnames(out) <- x$teams
  out
}


#' @rdname elo.run.helpers
#' @export
as.data.frame.elo.run <- function(x, ...)
{
  out <- as.data.frame(x$elos)
  colnames(out) <- c("team.A", "team.B", "p.A", "wins.A", "update", "elo.A", "elo.B")
  out$team.A <- factor(out$team.A, levels = seq_along(x$teams), labels = x$teams)
  out$team.B <- factor(out$team.B, levels = seq_along(x$teams), labels = x$teams)
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
  check_final_elos(x, length(x$teams))
  out <- finalElos(x$elos, length(x$teams))
  names(out) <- x$teams
  out
}


