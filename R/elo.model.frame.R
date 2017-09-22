
#' Interpret formulas in \code{elo} functions
#'
#' A helper function to create the \code{model.frame} for many \code{elo} functions.
#'
#' @param formula A formula. See "details", below.
#' @param data A \code{data.frame} in which to look for objects in \code{formula}.
#' @param na.action A function which indicates what should happen when the data contain NAs.
#' @param subset An optional vector specifying a subset of observations.
#' @param k A constant k-value (or a vector, where appropriate).
#' @param ... Other arguments (not in use at this time).
#' @param required.vars One or more of \code{c("wins", "elos", "k", "group", "regress")},
#'   denoting which variables are required to appear in the final model.frame.
#' @details
#' \code{formula} is usually of the form \code{wins.A ~ elo.A + elo.B}, where \code{elo.A} and \code{elo.B}
#'   are vectors of Elos, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A (Elo A) won or lost (or something between). \code{elo.prob} also allows
#'   \code{elo.A} and \code{elo.B} to be character or factors, denoting which team(s) played. \code{elo.run}
#'   requires \code{elo.A} to be a vector of teams (sometimes denoted by \code{"team.A"}),
#'   but \code{elo.B} can be either a vector of teams or  else a numeric column
#'   (denoting a fixed-Elo opponent).
#'
#' \code{formula} accepts two special functions in it. \code{k()} allows for complicated Elo updates. For
#'   constant Elo updates, use the \code{k = } argument instead of this special function.
#'   \code{adjust()} allows for Elos to be adjusted for, e.g., home-field advantage. The second argument
#'   to this function can be a scalar or vector of appropriate length.
#'
#' @seealso \code{\link{elo.run}}, \code{\link{elo.calc}}, \code{\link{elo.prob}}
#' @export
elo.model.frame <- function(formula, data, na.action, subset, k = NULL, ..., required.vars = "elos")
{
  Call <- match.call()
  required.vars <- match.arg(required.vars, c("wins", "elos", "k", "group", "regress"), several.ok = TRUE)
  indx <- match(c("formula", "data", "subset", "na.action"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required.")

  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::model.frame)
  specials <- c("adjust", "k", "group", "regress")

  temp.call$formula <- if(missing(data))
  {
    stats::terms(formula, specials)
  } else stats::terms(formula, specials, data = data)

  adjenv <- new.env(parent = environment(formula))
  if(!is.null(attr(temp.call$formula, "specials")$adjust))
  {
    assign("adjust", function(x, y) {
      if(length(y) == 1)
      {
        attr(x, "adjust") <- rep(y, times = length(x))
      } else if(length(y) == length(x))
      {
        attr(x, "adjust") <- y
      } else stop("The second argument to 'adjust' needs to be length 1 or the same length as the first argument.")

      class(x) <- c("adjustedElo", class(x))
      x
    }, envir = adjenv)
  }
  if(!is.null(attr(temp.call$formula, "specials")$k))
  {
    assign("k", function(x) x, envir = adjenv)
  }
  if(!is.null(attr(temp.call$formula, "specials")$group))
  {
    assign("group", function(x) x, envir = adjenv)
  }
  if(!is.null(attr(temp.call$formula, "specials")$regress))
  {
    assign("regress", function(x, to, by) {
      attr(x, "to") <- to
      attr(x, "by") <- by
      class(x) <- c("regressElo", class(x))
      x
    }, envir = adjenv)
  }
  environment(temp.call$formula) <- adjenv

  mf <- eval(temp.call, parent.frame())
    if(nrow(mf) == 0) stop("No (non-missing) observations")

  Terms <- stats::terms(mf)

  #####################################################################

  empty <- function(x) is.null(x) || length(x) == 0
  has.wins <- attr(Terms, "response") == 1

  k.col <- attr(Terms, "specials")$k
  has.k <- !empty(k.col) || !is.null(k)

  grp.col <- attr(Terms, "specials")$group
  reg.col <- attr(Terms, "specials")$regress

  if("wins" %in% required.vars && !has.wins)
  {
    stop("A 'wins' component is required in the left-hand side of 'formula'.")
  }

  if("k" %in% required.vars && !has.k)
  {
    stop("'k' is not in 'formula' or specified as an argument.")
  } else if(!empty(k.col) && !is.null(k))
  {
    warning("'k = ' argument being ignored.")
  }

  sum.empty <- !empty(k.col) + !empty(grp.col) + !empty(reg.col)
  if(has.wins + sum.empty + 2 != ncol(mf))
  {
    stop("'formula' not specified correctly: found ", ncol(mf), " columns; expected ",
         has.wins + sum.empty + 2)
  }

  # figure out which columns are the "real" ones
  elo.cols <- if(sum.empty == 0)
  {
    (1:2) + has.wins
  } else setdiff(1:ncol(mf), c(if(has.wins) 1, k.col, grp.col, reg.col))
  if(length(elo.cols) != 2) stop("Trouble finding the Elo columns.")

  #####################################################################

  out <- data.frame(
    elo.A = remove_adjustedElo(mf[[elo.cols[1]]]),
    elo.B = remove_adjustedElo(mf[[elo.cols[2]]])
  )

  if("wins" %in% required.vars) out$wins.A <- validate_score(as.numeric(mf[[1]]))
  if("k" %in% required.vars)
  {
    out$k <- if(empty(k.col)) k else mf[[k.col]]
    if(!is.numeric(out$k) || anyNA(out$k)) stop("'k' should be numeric and non-NA.")
  }
  if("group" %in% required.vars)
  {
    out$group <- if(empty(grp.col)) TRUE else mf[[grp.col]]
  }
  if("regress" %in% required.vars)
  {
    out$regress <- if(empty(reg.col)) FALSE else mf[[reg.col]]
  }

  adjs <- attr(Terms, "specials")$adjust
  out$adj.A <- if(empty(adjs) || !any(adjs == elo.cols[1])) 0 else attr(mf[[elo.cols[1]]], "adjust")
  out$adj.B <- if(empty(adjs) || !any(adjs == elo.cols[2])) 0 else attr(mf[[elo.cols[2]]], "adjust")

  if(!is.numeric(out$adj.A) || !is.numeric(out$adj.B)) stop("Any Elo adjustments should be numeric!")

  attr(out, "terms") <- Terms

  return(out)
}
