
#' Calculate Elos for a series of matches
#'
#' Calculate Elos for a series of matches.
#'
#' @inheritParams elo.model.frame
#' @param k A constant k-value. See "details", below.
#' @param initial.elo An optional named vector containing initial Elo ratings for all teams in \code{formula}.
#' @param ... Other arguments (not used at this time).
#' @param x An object of class \code{"elo.run"}.
#' @return An object of class \code{"elo.run"}.
#' @details
#' \code{formula} is usually of the form \code{wins.A ~ team.A + team.B}, where \code{team.A} and \code{team.B}
#'   are character vectors or factors denoting which two teams played, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A won or lost (or something between).
#'
#' It is also acceptable for either \code{team.A} or \code{team.B} to be a numeric column (if, for example,
#'   the Elo of one team or the other is known or fixed). If both are numeric, a warning will be issued,
#'   and results will be calculated using \code{\link{elo.calc}}.
#'
#' \code{formula} accepts two special functions in it. \code{k()} allows for complicated Elo updates. For
#'   constant Elo updates, use the \code{k = } argument instead of this special function.
#'   \code{adjust()} allows for Elos to be adjusted for, e.g., home-field advantage. The second argument
#'   to this function can be a scalar or vector of appropriate length. See the examples.
#'
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
#' @seealso \code{\link{elo.run}}
#' @name elo.run
NULL
#> NULL

#' @rdname elo.run
#' @export
elo.run <- function(formula, data, na.action, subset, k = NULL, initial.elo = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$envir <- parent.frame()
  mf <- eval(Call, parent.frame())
  Terms <- stats::terms(mf)

  k.col <- attr(stats::terms(mf), "specials")$k
  if(is.null(k.col))
  {
    if(ncol(mf) != 3) stop("'formula' doesn't appear to be specified correctly.")
    if(is.null(k)) stop("'k' is not in 'formula' or specified as numeric constant.")
    if(!is.numeric(k) || length(k) != 1 || anyNA(k)) stop("'k' should be a numeric constant.")
    mf$`(k)` <- rep(k, times = nrow(mf))
  } else
  {
    if(ncol(mf) != 4) stop("'formula' doesn't appear to be specified correctly.")
    if(!identical(k.col, 4L)) stop("'k()' should be the last term in 'formula'.")
    colnames(mf)[4L] <- "(k)"
  }

  adjs <- attr(Terms, "specials")$adjust
  mf$`(adj1)` <- if(is.null(adjs) || !any(adjs == 2)) 0 else attr(mf[[2]], "adjust")
  mf$`(adj2)` <- if(is.null(adjs) || !any(adjs == 3)) 0 else attr(mf[[3]], "adjust")

  checked <- check_elo_vars(mf, initial.elo)

  if(checked$flag == 3)
  {
    warning("Both teams are detected as numeric. Will revert to elo.calc()")
    return(elo.calc(checked$team.A + checked$adj.team.A,
                    checked$team.B + checked$adj.team.B,
                    checked$wins.A, checked$k))
  }

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

