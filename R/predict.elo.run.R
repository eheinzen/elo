
#' Make Predictions on an \code{elo.run} Object
#'
#' @param object An object of class \code{"\link{elo.run}"}.
#' @param newdata A new dataset containing the same variables as the call
#'   that made \code{object}. If missing, the predicted win probabilities from
#'   \code{object} will be returned.
#' @param ... Other arguments to be passed to \code{\link{elo.prob}}.
#' @return A vector of win probabilities.
#' @examples
#' data(tournament)
#' t1 <- head(tournament, -3)
#' t2 <- tail(tournament, 3)
#' results <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'                    data = t1, k = 20)
#' predict(results)
#' predict(results, newdata = t2)
predict.elo.run <- function(object, newdata, ...)
{
  if(missing(newdata))
  {
    e <- object$elos
    g <- e[, 1]
    return(e[g > 0 & !duplicated(g), 4])
  } else
  {
    return(elo.prob(object$terms, data = newdata, ..., elos = last(object)))
  }
}
