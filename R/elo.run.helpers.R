
#' Helper functions for \code{elo.run}
#'
#' \code{as.matrix} converts an Elo object into a matrix of running Elos. These are the Elos at the time of grouping,
#'   but before any regression takes place.
#'
#' \code{as.data.frame} converts the \code{"elos"} component of an object
#'   from \code{\link{elo.run}} into a data.frame.
#'
#' \code{final.elos} is a generic function to extract the last Elo per team.
#'
#' @param x An object of class \code{"elo.run"} or class \code{"elo.run.regressed"}.
#' @param ... Other arguments (Not in use at this time).
#' @param regressed Logical, denoting whether to use the post-regressed (\code{TRUE}) or
#'   pre-regressed (\code{FALSE}) final Elos. Note that \code{TRUE} only makes sense when the
#'   final Elos were regressed one last time (i.e., if the last element of the \code{regress()})
#'   vector yields \code{TRUE}).
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
as.matrix.elo.run <- function(x, ...)
{
  group <- check_as_matrix(x, x$group)
  out <- eloRunAsMatrix(x$elos, x$initial.elos, group)
  colnames(out) <- x$teams
  out
}

#' @rdname elo.run.helpers
#' @export
as.matrix.elo.run.regressed <- function(x, ...)
{
  group <- check_as_matrix(x, x$group, regr = TRUE)
  out <- eloRunRegressedAsMatrix(x$elos, x$initial.elos, x$elos.regressed,
                                 check_group_regress(x$regress),
                                 group)
  colnames(out) <- x$teams
  out
}

#' @rdname elo.run.helpers
#' @export
as.data.frame.elo.run <- function(x, ...)
{
  out <- as.data.frame(x$elos)
  nm.a <- if(x$n.players[1] > 1) paste0(".", seq_len(x$n.players[1])) else ""
  nm.b <- if(x$n.players[2] > 1) paste0(".", seq_len(x$n.players[2])) else ""

  colnames(out) <- c(paste0("team.A", nm.a), paste0("team.B", nm.b),
                     "p.A", "wins.A", "update.A", "update.B",
                     paste0("elo.A", nm.a), paste0("elo.B", nm.b))
  out[paste0("team.A", nm.a)] <- lapply(out[paste0("team.A", nm.a)], factor, levels = seq_along(x$teams), labels = x$teams)
  out[paste0("team.B", nm.b)] <- lapply(out[paste0("team.B", nm.b)], factor, levels = seq_along(x$teams), labels = x$teams)
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

#' @rdname elo.run.helpers
#' @export
final.elos.elo.run.regressed <- function(x, regressed = FALSE, ...)
{
  if(regressed && !utils::tail(check_group_regress(x$regress), 1))
    stop("'regressed = TRUE' only makes sense if the final Elos are regressed after the final game.")

  if(!regressed) return(NextMethod())

  out <- x$elos.regressed[nrow(x$elos.regressed), ]
  names(out) <- x$teams
  out
}

