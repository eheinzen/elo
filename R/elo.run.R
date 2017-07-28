
#' Calculate Elos for a series of matches
#'
#' Calculate Elos for a series of matches.
#'
#' @param formula A formula of the form \code{wins.A ~ team.A + team.B}, where \code{team.A} and \code{team.B}
#'   are character vectors or factors denoting which two teams played, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A won or lost (or something between). The teams can be adjusted for other variables.
#'   See "details", below.
#' @param data A \code{data.frame} in which to look for objects in \code{formula} and \code{k}.
#' @param k The k-value, specified as a vector. This allows for complicated Elo updates. See "details", below.
#' @param na.action A function which indicates what should happen when the data contain NAs.
#' @param subset An optional vector specifying a subset of observations.
#' @param initial.elo An optional named vector containing initial Elo ratings for all teams in \code{formula}.
#' @param ... Other arguments (not used at this time).
#' @examples
#' data(tournament)
#' tournament$k <- 20
#' elo.calc(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament, k = k)
#' @export
elo.run <- function(formula, data, k, na.action, subset, initial.elo = NULL, ...)
{
  Call <- match.call()

  indx <- match(c("formula", "data", "subset", "na.action", "k"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required.")
  if(indx[5] == 0) stop("Please specify the 'k' value column (or vector).")

  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::model.frame)
  temp.call$formula <- if(missing(data)) terms(formula, "adjust") else terms(formula, "adjust", data = data)


  if (!is.null(attr(temp.call$formula, "specials")$adjust))
  {
    adjenv <- new.env(parent = environment(formula))
    assign("adjust", function(x, y){attr(x, "adjust") <- y; class(x) <- c("adjustedElo", class(x)); x}, env = adjenv)
    environment(temp.call$formula) <- adjenv
  }

  mf <- eval(temp.call, parent.frame())

  if(nrow(mf) == 0) stop("No (non-missing) observations")
  if(ncol(mf) != 4) stop("The left-hand side of 'formula' should have one term and the right-hand side should have two terms.")

  adjs <- attr(terms(mf), "specials")$adjust
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
  colnames(out) <- names(checked$initial.elo)

  return(structure(list(elos = out, model.frame = mf), class = "elo.calc"))
}








