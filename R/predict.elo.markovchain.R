
#' Make Predictions on an \code{elo.glm} Object
#'
#' @param object An object of class \code{"\link{elo.markovchain}"}.
#' @param newdata A new dataset containing the same variables as the call
#'   that made \code{object}. If missing, the predicted win probabilities from
#'   \code{object} will be returned.
#' @param ... Other arguments.
#' @return A vector of win probabilities.
#' @examples
#' data(tournament)
#' t1 <- head(tournament, -3)
#' t2 <- tail(tournament, 3)
#' results <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = t1,
#'   subset = points.Home != points.Visitor, k = 0.7)
#' predict(results)
#' predict(results, newdata = t2)
#' @name predict.elo.markovchain
NULL
#> NULL

#' @rdname predict.elo.markovchain
#' @export
predict.elo.markovchain <- function(object, newdata, ...)
{
  if(missing(newdata)) return(fitted(object))
  form <- clean_elo_formula(object$elo.terms)
  mf <- elo.model.frame(form, data = newdata, required.vars = "elos")
  dat <- data.frame(difference = unname(object$pi[as.character(mf$elo.A)] - object$pi[as.character(mf$elo.B)]))
  stats::predict.glm(object$fit, newdata = dat, type = "response", ...)
}
