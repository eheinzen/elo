
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
#' \code{formula} accepts four special functions in it:
#'
#' \code{k()} allows for complicated Elo updates. For
#'   constant Elo updates, use the \code{k = } argument instead of this special function.
#'
#' \code{adjust()} allows for Elos to be adjusted for, e.g., home-field advantage. The second argument
#'   to this function can be a scalar or vector of appropriate length.
#'
#' \code{regress()} can be used to regress Elos back to a fixed value
#'   after certain matches. Giving a logical vector identifies these matches after which to
#'   regress back to the mean. Giving any other kind of vector regresses after the appropriate
#'   groupings (see, e.g., \code{\link{duplicated}(..., fromLast = TRUE)}). The other three arguments determine
#'   what Elo to regress to (\code{to = }), by how much to regress toward that value
#'   (\code{by = }), and whether to continue regressing teams which have stopped playing (\code{regress.unused},
#'   default = \code{TRUE}).
#'
#' \code{group()} is used to group matches (by, e.g., week). It is fed to \code{\link{as.matrix.elo.run}}
#'   to produce only certain rows of matrix output.
#'
#' @seealso \code{\link{elo.run}}, \code{\link{elo.calc}}, \code{\link{elo.update}}, \code{\link{elo.prob}}
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
    assign("regress", function(x, to, by, regress.unused = TRUE) {
      if(!is.numeric(to) || length(to) != 1 || anyNA(to)) stop("regress: 'to' must be numeric.")
      if(!is.numeric(by) || length(by) != 1 || anyNA(by) || by > 1 || by < 0)
        stop("regress: 'by' must be 0 <= by <= 1")
      if(!is.logical(regress.unused) || length(regress.unused) != 1 || anyNA(regress.unused))
        stop("regress: 'regress.unused' must be a single logical value.")
      attr(x, "to") <- to
      attr(x, "by") <- by
      attr(x, "regress.unused") <- regress.unused
      class(x) <- c("regressElo", class(x))
      x
    }, envir = adjenv)
  }
  environment(temp.call$formula) <- adjenv

  mf <- eval(temp.call, parent.frame())
    if(nrow(mf) == 0) stop("No (non-missing) observations")

  Terms <- stats::terms(mf)

  #####################################################################

  has.wins <- attr(Terms, "response") == 1

  k.col <- attr(Terms, "specials")$k
  has.k <- !null_or_length0(k.col) || !is.null(k)

  grp.col <- attr(Terms, "specials")$group
  reg.col <- attr(Terms, "specials")$regress

  if("wins" %in% required.vars && !has.wins)
  {
    stop("A 'wins' component is required in the left-hand side of 'formula'.")
  }

  if("k" %in% required.vars && !has.k)
  {
    stop("'k' is not in 'formula' or specified as an argument.")
  } else if(!null_or_length0(k.col) && !is.null(k))
  {
    warning("'k = ' argument being ignored.")
  }

  # need all the parens b/c ! is a low-precident operator
  sum.empty <- (!null_or_length0(k.col)) + (!null_or_length0(grp.col)) + (!null_or_length0(reg.col))

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
    out$k <- if(null_or_length0(k.col)) k else mf[[k.col]]
    if(!is.numeric(out$k) || anyNA(out$k)) stop("'k' should be numeric and non-NA.")
  }
  if("group" %in% required.vars)
  {
    out$group <- if(null_or_length0(grp.col)) TRUE else mf[[grp.col]]
  }
  if("regress" %in% required.vars)
  {
    out$regress <- if(null_or_length0(reg.col)) FALSE else mf[[reg.col]]
    if(null_or_length0(reg.col))
    {
      attr(out$regress, "to") <- 1500
      attr(out$regress, "by") <- 0
      attr(out$regress, "regress.unused") <- FALSE
    }
  }

  adjs <- attr(Terms, "specials")$adjust
  out$adj.A <- if(null_or_length0(adjs) || !any(adjs == elo.cols[1])) 0 else attr(mf[[elo.cols[1]]], "adjust")
  out$adj.B <- if(null_or_length0(adjs) || !any(adjs == elo.cols[2])) 0 else attr(mf[[elo.cols[2]]], "adjust")

  if(!is.numeric(out$adj.A) || !is.numeric(out$adj.B)) stop("Any Elo adjustments should be numeric!")

  attr(out, "terms") <- Terms

  return(out)
}
