
#' Calculate Elos for a series of matches
#'
#' Calculate Elos for a series of matches.
#'
#' @inheritParams elo.calc
#' @param initial.elos An optional named vector containing initial Elo ratings for all teams in \code{formula}.
#' @param ... Other arguments (not used at this time).
#' @param x An object of class \code{"elo.run"}.
#' @return An object of class \code{"elo.run"}.
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
elo.run <- function(formula, data, na.action, subset, k = NULL, initial.elos = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k", "group", "regress")
  mf <- eval(Call, parent.frame())
  Terms <- stats::terms(mf)


  checked <- check_elo_run_vars(mf, initial.elos)
  out <- eloRun(checked$team.A,
                checked$team.B,
                checked$wins.A,
                checked$k,
                checked$adj.A,
                checked$adj.B,
                checked$initial.elos,
                checked$flag)
  colnames(out) <- c("game", "team", "elo", "p.Win", "wins")

  return(structure(list(elos = out,
                        teams = names(checked$initial.elos),
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

