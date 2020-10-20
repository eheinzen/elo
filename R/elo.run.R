#' Calculate running Elos for a series of matches.
#'
#' @inheritParams elo.calc
#' @param initial.elos An optional named vector containing initial Elo ratings for all teams in \code{formula}.
#'   If a single (unnamed) value is supplied, that value is applied to all teams. \code{NULL} (the default)
#'   sets all Elos to 1500.
#' @param ... Other arguments (not used at this time).
#' @param prob.fun A function with at least 4 arguments: elo.A, elo.B, adjust.A, and adjust.B. It should return a predicted probability
#'   that team A wins. The values passed in will be scalars, and a scalar is expected as output.
#' @param update.fun A function with at least 6 arguments: the same as \code{\link{elo.update.default}}. The function takes
#'   in the Elos, the win indicator, k, and any adjustments, and returns a value by which to update the Elos. The values passed in
#'   will be scalars, and a scalar is expected as output.
#' @param verbose Should a message be issued when R is used (over C++)?
#' @details
#'   \code{elo.run} is run two different ways: the first (default) uses C++ and may be up to 50 times faster,
#'   while the second (when \code{prob.fun} or \code{update.fun} are specified) uses R but also supports custom update functions.
#'   Prefer the first unless you really need a custom update function.
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
elo.run <- function(formula, data, na.action, subset, k = NULL, initial.elos = NULL, ..., prob.fun = elo.prob, update.fun = elo.update, verbose = TRUE)
{
  Call <- match.call()
  Call <- Call[c(1, match(c("formula", "data", "subset", "na.action", "k"), names(Call), nomatch = 0))]
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k", "group", "regress")
  Call$ncol.k <- 2
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  checked <- check_elo_run_vars(mf, initial.elos)
  if(missing(prob.fun) && missing(update.fun))
  {
    out <- do.call(eloRun, checked)
  } else
  {
    if(verbose) message("Using R instead of C++")
    checked$prob.fun <- match.fun(prob.fun)
    checked$update.fun <- match.fun(update.fun)
    out <- do.call(eloRun2, checked)
  }
  any.regr <- any(checked$regress)

  structure(list(
    elos = out[[1]],
    n.matches = nrow(out[[1]]),
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
  cat("\nAn object of class '", class(x)[1], "', containing information on ",
      length(x$teams), " teams and ", x$n.matches, " matches.\n\n", sep = "")
  invisible(x)
}


#' @export
print.elo.run.regressed <- function(x, ...)
{
  cat("\nAn object of class '", class(x)[1], "', containing information on ",
      length(x$teams), " teams and ", x$n.matches, " matches, with ",
      nrow(x$elos.regressed), " regressions.\n\n", sep = "")
  invisible(x)
}
