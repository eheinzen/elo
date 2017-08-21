
#' Interpret formulas in \code{elo} functions
#'
#' A helper function to create the model frame for many \code{elo} functions.
#'
#' @param formula A formula. See "details", below.
#' @param data A \code{data.frame} in which to look for objects in \code{formula}.
#' @param na.action A function which indicates what should happen when the data contain NAs.
#' @param subset An optional vector specifying a subset of observations.
#' @param ... Other arguments (not in use at this time).
#' @param envir An environment in which to evaluate \code{\link[stats]{model.frame}}. Almost always
#'   \code{\link{parent.frame}}.
#' @seealso \code{\link{elo.run}}, \code{\link{elo.calc}}, \code{\link{elo.prob}}
#' @export
elo.model.frame <- function(formula, data, na.action, subset, ..., envir)
{
  Call <- match.call()

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


  mf <- eval(temp.call, envir)

  if(nrow(mf) == 0) stop("No (non-missing) observations")
  return(mf)
}



