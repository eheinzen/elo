
#' Rank teams
#'
#' Extract the rankings from Elo objects.
#'
#' @param object An object.
#' @param ties.method Passed to \code{\link{rank}}.
#' @param regressed Passed to \code{\link{final.elos}}.
#' @param ... Other arguments
#' @name rank.teams
NULL
#> NULL

#' @rdname rank.teams
#' @export
rank.teams <- function(object, ties.method = "min", ...)
{
  UseMethod("rank.teams")
}

#' @rdname rank.teams
#' @export
rank.teams.elo.run <- function(object, ties.method = "min", ...)
{
  rank(-final.elos(object), ties.method = ties.method)
}

#' @rdname rank.teams
#' @export
rank.teams.elo.run.regressed <- function(object, ties.method = "min", regressed = FALSE, ...)
{
  rank(-final.elos(object, regressed = regressed), ties.method = ties.method)
}

#' @rdname rank.teams
#' @export
rank.teams.elo.glm <- function(object, ties.method = "min", ...)
{
  coeff <- stats::setNames(object$coefficients, setdiff(names(object$data), "wins.A"))
  others <- setdiff(object$teams, names(coeff))
  others <- stats::setNames(rep(0, length(others)), others)
  rank(-c(coeff, others)[object$teams], ties.method = ties.method)
}

#' @rdname rank.teams
#' @export
rank.teams.elo.markovchain <- function(object, ties.method = "min", ...)
{
  rank(-object$pi, ties.method = ties.method)
}

#' @rdname rank.teams
#' @export
rank.teams.elo.winpct <- function(object, ties.method = "min", ...)
{
  rank(-object$win.pct, ties.method = ties.method)
}

#' @rdname rank.teams
#' @export
rank.teams.elo.colley <- function(object, ties.method = "min", ...)
{
  rank(-object$pi, ties.method = ties.method)
}




