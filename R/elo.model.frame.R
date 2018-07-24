
#' Interpret formulas in \code{elo} functions
#'
#' A helper function to create the \code{model.frame} for many \code{elo} functions.
#'
#' @param formula A formula. See \link[=formula.specials]{the help page for formulas} for details.
#' @param data A \code{data.frame} in which to look for objects in \code{formula}.
#' @param na.action A function which indicates what should happen when the data contain NAs.
#' @param subset An optional vector specifying a subset of observations.
#' @param k A constant k-value (or a vector, where appropriate).
#' @param ... Other arguments (not in use at this time).
#' @param required.vars One or more of \code{c("wins", "elos", "k", "group", "regress")},
#'   denoting which variables are required to appear in the final model.frame.
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
  specials <- c("adjust", "k", "group", "regress", "players")

  temp.call$formula <- if(missing(data))
  {
    stats::terms(formula, specials)
  } else stats::terms(formula, specials, data = data)

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

  out <- data.frame(row.names = 1:nrow(mf)) # in case one of the next two lines is a matrix
  out$elo.A <- remove_elo_adjust(mf[[elo.cols[1]]])
  out$elo.B <- remove_elo_adjust(mf[[elo.cols[2]]])

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
    out$regress <- if(null_or_length0(reg.col))
    {
      regress(rep(FALSE, times = nrow(out)), 1500, 0, FALSE)
    } else mf[[reg.col]]
  }

  adjs <- attr(Terms, "specials")$adjust
  out$adj.A <- if(null_or_length0(adjs) || !any(adjs == elo.cols[1])) 0 else attr(mf[[elo.cols[1]]], "adjust")
  out$adj.B <- if(null_or_length0(adjs) || !any(adjs == elo.cols[2])) 0 else attr(mf[[elo.cols[2]]], "adjust")

  if(!is.numeric(out$adj.A) || !is.numeric(out$adj.B)) stop("Any Elo adjustments should be numeric!")

  attr(out, "terms") <- Terms

  out
}
