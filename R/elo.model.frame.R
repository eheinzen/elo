
#' Interpret formulas in \code{elo} functions
#'
#' A helper function to create the model frame for many \code{elo} functions.
#'
#' @param formula A formula. See "details", below.
#' @param data A \code{data.frame} in which to look for objects in \code{formula}.
#' @param na.action A function which indicates what should happen when the data contain NAs.
#' @param subset An optional vector specifying a subset of observations.
#' @param k A constant k-value (or a vector, where appropriate). See "details", below.
#' @param ... Other arguments (not in use at this time).
#' @param required.vars One or more of \code{c("wins", "teams", "k")}, denoting which variables
#'   are required to appear in the final model.frame..
#' @seealso \code{\link{elo.run}}, \code{\link{elo.calc}}, \code{\link{elo.prob}}
#' @export
elo.model.frame <- function(formula, data, na.action, subset, k = NULL, ..., required.vars = "teams")
{
  Call <- match.call()
  required.vars <- match.arg(required.vars, c("wins", "teams", "k"), several.ok = TRUE)
  indx <- match(c("formula", "data", "subset", "na.action"), names(Call), nomatch = 0)
  if(indx[1] == 0) stop("A formula argument is required.")

  temp.call <- Call[c(1, indx)]
  temp.call[[1L]] <- quote(stats::model.frame)
  specials <- c("adjust", "k")

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
  environment(temp.call$formula) <- adjenv

  mf <- eval(temp.call, parent.frame())
    if(nrow(mf) == 0) stop("No (non-missing) observations")

  Terms <- stats::terms(mf)

  #####################################################################

  has.wins <- attr(Terms, "response") == 1
  if("wins" %in% required.vars && !has.wins)
  {
    stop("A 'wins' component is required in 'formula'.")
  } else if("wins" %in% required.vars)
  {
    mf[[1]] <- as.numeric(mf[[1]])
    validate_score(mf[[1]])
  }

  #####################################################################

  k.col <- attr(Terms, "specials")$k
  has.k <- !is.null(k.col) || !is.null(k)

  if(!has.k && "k" %in% required.vars) stop("'k' is not in 'formula' or specified as an argument.")

  if(is.null(k.col) && !is.null(k))
  {
    if(ncol(mf) != 2 + has.wins) stop("'formula' doesn't appear to be specified correctly.")
    mf$`(k)` <- k
    k.col <- 3 + has.wins
  } else if(!is.null(k.col))
  {
    if(!is.null(k)) warning("'k = ' argument being ignored.")
    if(ncol(mf) != 3 + has.wins) stop("'formula' doesn't appear to be specified correctly.")
    if(!identical(k.col, as.integer(3 + has.wins))) stop("'k()' should be the last term in 'formula'.")
  } else
  {
    if(ncol(mf) != 2 + has.wins) stop("'formula' doesn't appear to be specified correctly.")
  }

  if("k" %in% required.vars && (!is.numeric(mf[[k.col]]) || anyNA(mf[[k.col]])))
    stop("'k' should be numeric and non-NA.")

  #####################################################################

  adjs <- attr(Terms, "specials")$adjust
  mf$`(adj1)` <- if(is.null(adjs) || !any(adjs == 1 + has.wins)) 0 else attr(mf[[1 + has.wins]], "adjust")
  mf$`(adj2)` <- if(is.null(adjs) || !any(adjs == 2 + has.wins)) 0 else attr(mf[[2 + has.wins]], "adjust")

  if(!is.numeric(mf$`(adj1)`) || !is.numeric(mf$`(adj2)`)) stop("Any Elo adjustments should be numeric!")

  #####################################################################

  attr(mf, "has.wins") <- has.wins
  attr(mf, "has.k") <- has.k

  if(4 + has.wins + has.k != ncol(mf)) stop("Something went wrong parsing the formula into a model.frame.")

  return(mf)
}

has.wins <- function(x)
{
  attr(x, "has.wins")
}

