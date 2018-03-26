
#' Make Predictions on an \code{elo.run} Object
#'
#' @param object An object of class \code{"\link{elo.run}"}.
#' @param newdata A new dataset containing the same variables as the call
#'   that made \code{object}. If missing, the predicted win probabilities from
#'   \code{object} will be returned.
#' @param regressed See the note on \code{\link{final.elos}}.
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
#' @name predict.elo.run
NULL
#> NULL

#' @rdname predict.elo.run
#' @export
predict.elo.run <- function(object, newdata, ...)
{
  if(missing(newdata))
  {
    return(fitted(object))
  } else
  {
    form <- clean_elo_formula(stats::terms(object))
    return(elo.prob(form, data = newdata, ..., elos = final.elos(object)))
  }
}


#' @rdname predict.elo.run
#' @export
predict.elo.run.regressed <- function(object, newdata, regressed = FALSE, ...)
{
  if(missing(newdata))
  {
    return(fitted(object))
  } else
  {
    form <- clean_elo_formula(stats::terms(object))
    return(elo.prob(form, data = newdata, ..., elos = final.elos(object, regressed = regressed)))
  }
}
