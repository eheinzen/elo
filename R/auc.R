
#' Calculate AUC on an \code{elo.run} object
#'
#' @param object An object of class \code{\link{elo.run}}.
#' @inheritParams mse
#' @references Adapted from code here:
#'   \url{https://stat.ethz.ch/pipermail/r-help/2005-September/079872.html}
#' @return The AUC of the predicted Elo probabilities and the actual win results.
#' @seealso \code{pROC::\link[pROC]{auc}}, \code{\link{elo.run}}.
#' @name auc.elo
NULL
#> NULL

get_auc <- function(wins, probs)
{
  x.won <- probs[wins == 1]
  x.lost <- probs[wins == 0]

  n.won <- 0 + length(x.won) # to coerce to double
  if(n.won == 0 || length(x.lost) == 0) stop("Unable to calculate AUC: need both 0 and 1 in the wins column.")
  (sum(rank(c(x.won, x.lost))[1:n.won]) - n.won*(n.won + 1)/2)/(n.won * length(x.lost))
}

#' @rdname auc.elo
#' @export
auc.elo.run <- function(object, ..., subset = TRUE)
{
  probs <- fitted(object)
  wins <- object$elos[, sum(object$n.players) + 2]
  get_auc(wins[subset], probs[subset])
}

#' @rdname auc.elo
#' @export
auc.elo.glm <- function(object, ..., subset = TRUE)
{
  get_auc(object$y[subset], object$fitted.values[subset])
}

#' @rdname auc.elo
#' @export
auc.elo.running <- function(object, running = TRUE, discard.skipped = FALSE, ..., subset = TRUE)
{
  if(!running) return(NextMethod())
  if(!is.logical(subset)) stop("'subset' must be logical for this functionality")
  idx <- attr(object$running.values, "group") > (if(discard.skipped) 0 else -1)
  get_auc(object$y[idx & subset], object$running.values[idx & subset])
}

#' @rdname auc.elo
#' @export
auc.elo.markovchain <- auc.elo.glm

#' @rdname auc.elo
#' @export
auc.elo.winpct <- auc.elo.glm

#' @rdname auc.elo
#' @export
auc.elo.colley <- auc.elo.glm
