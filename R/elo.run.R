
#' Calculate Elos for a series of matches
#'
#' Calculate Elos for a series of matches.
#'
#' @param formula A formula. See "details", below.
#' @inheritParams elo.model.frame
#' @param initial.elo An optional named vector containing initial Elo ratings for all teams in \code{formula}.
#' @param ... Other arguments (not used at this time).
#' @param x An object of class \code{"elo.run"}.
#' @return An object of class \code{"elo.run"}.
#' @details
#' The formula in this function is slightly different from the other elo package functions.
#'   Here, \code{formula} is usually of the form \code{wins.A ~ team.A + team.B},
#'   where \code{team.A} and \code{team.B} are character vectors or factors denoting
#'   which two teams played, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A won or lost (or something between).
#'
#' It is also acceptable for either \code{team.A} or \code{team.B} to be a numeric column (if, for example,
#'   the Elo of one team or the other is known or fixed). If both are numeric, a warning will be issued,
#'   and results will be calculated using \code{\link{elo.calc}}.
#'
#' The special functions documented in \code{\link{elo.model.frame}} are still valid here
#'   (perhaps moreso than in the other functions!).
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
#' elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
#'         data = tournament, k = 20)
#'
#' tournament$home.field <- 10
#' elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, home.field) + team.Visitor,
#'         data = tournament, k = 20)
#'
#' @seealso \code{\link{score}}, \code{\link{elo.calc}}, \code{\link{elo.update}}, \code{\link{elo.prob}}
#' @name elo.run
NULL
#> NULL

#' @rdname elo.run
#' @export
elo.run <- function(formula, data, na.action, subset, k = NULL, initial.elo = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$required.vars <- c("wins", "teams", "k")
  mf <- eval(Call, parent.frame())
  Terms <- stats::terms(mf)


  checked <- check_elo_run_vars(mf, initial.elo)
  out <- eloRun(checked$team.A,
                checked$team.B,
                checked$wins.A,
                checked$k,
                checked$adj.team.A,
                checked$adj.team.B,
                checked$initial.elo,
                checked$flag)
  colnames(out) <- c("game", "team", "elo", "p.Win", "wins")

  return(structure(list(elos = out,
                        teams = names(checked$initial.elo),
                        terms = Terms), class = "elo.run"))
}


#' @rdname elo.run
#' @export
print.elo.run <- function(x, ...)
{
  cat("\nAn object of class 'elo.run', containing information on ",
      length(x$teams), " teams and ", max(x$elos[, 1]), " matches.\n\n", sep = "")
  invisible(x)
}

