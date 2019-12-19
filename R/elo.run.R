
#' \code{elo.run}
#'
#' Calculate Elos for a series of matches.
#'
#' @inheritParams elo.calc
#' @param initial.elos An optional named vector containing initial Elo ratings for all teams in \code{formula}.
#' @param ... Other arguments (not used at this time).
#' @return An object of class \code{"elo.run"} or class \code{"elo.run.regressed"}.
#' @examples
#' data(tournament)
#' elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'         data = tournament, k = 20)
#'
#' # Create non-constant 'k'
#' elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +
#'         k(20*log(abs(points.Home - points.Visitor) + 1)), data = tournament)
#'
#' # Adjust Elo for, e.g., home-field advantage
#' elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 30) + team.Visitor,
#'         data = tournament, k = 20)
#'
#' tournament$home.field <- 30
#' elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, home.field) + team.Visitor,
#'         data = tournament, k = 20)
#'
#' # Regress the Elos back toward 1500 at the end of the half-season
#' elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 30) +
#'         team.Visitor + regress(half, 1500, 0.2), data = tournament, k = 20)
#'
#' @seealso \code{\link{score}}, \link{elo.run.helpers}{elo.run helpers}, \code{\link{elo.calc}},
#'   \code{\link{elo.update}}, \code{\link{elo.prob}}, \code{\link{elo.model.frame}}.
#' @name elo.run
NULL
#> NULL

#' @rdname elo.run
#' @export
elo.run <- function(formula, data, na.action, subset, k = NULL, initial.elos = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k", "group", "regress")
  Call$ncol.k <- 2
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  checked <- check_elo_run_vars(mf, initial.elos)
  out <- do.call(eloRun, checked)
  any.regr <- any(checked$regress)

  structure(list(
    elos = out[[1]],
    n.players = c(ncol(checked$teamA), ncol(checked$teamB)),
    initial.elos = checked$initialElos,
    elos.regressed = if(any.regr) out[[2]] else NULL,
    teams = names(checked$initialElos),
    group = mf$group,
    regress = if(any.regr) mf$regress else NULL,
    terms = Terms,
    na.action = stats::na.action(mf)
  ), class = c(if(any.regr) "elo.run.regressed", "elo.run"))
}

#' @export
print.elo.run <- function(x, ...)
{
  cat("\nAn object of class 'elo.run', containing information on ",
      length(x$teams), " teams and ", nrow(x$elos), " matches.\n\n", sep = "")
  invisible(x)
}


#' @export
print.elo.run.regressed <- function(x, ...)
{
  cat("\nAn object of class 'elo.run.regressed', containing information on ",
      length(x$teams), " teams and ", nrow(x$elos), " matches, with ",
      nrow(x$elos.regressed), " regressions.\n\n", sep = "")
  invisible(x)
}
