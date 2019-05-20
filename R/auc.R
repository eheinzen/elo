
#' Calculate AUC on an \code{elo.run} object
#'
#' @param object An object of class \code{\link{elo.run}}.
#' @param ... Other arguments (not used at this time).
#' @references Adapted from code here:
#'   \url{https://stat.ethz.ch/pipermail/r-help/2005-September/079872.html}
#' @return The AUC of the predicted Elo probabilities and the actual win results.
#' @seealso \code{pROC::\link[pROC]{auc}}, \code{\link{elo.run}}.
#' @name elo.auc
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

#' @rdname elo.auc
#' @export
auc.elo.run <- function(object, ...)
{
  probs <- fitted(object)
  wins <- object$elos[, sum(object$n.players) + 2]
  get_auc(wins, probs)
}

#' @rdname elo.auc
#' @export
auc.elo.glm <- function(object, ...)
{
  get_auc(object$y, object$fitted.values)
}