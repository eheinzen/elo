
#' Make Predictions on an \code{elo.glm} Object
#'
#' @param object An object of class \code{"\link{elo.glm}"}.
#' @param newdata A new dataset containing the same variables as the call
#'   that made \code{object}. If missing, the predicted win probabilities from
#'   \code{object} will be returned.
#' @param type See \code{\link[stats]{predict.glm}}
#' @param running logical, denoting whether to use the running predicted values. Only makes
#'   sense if \code{newdata} is missing.
#' @param ... Other arguments.
#' @return A vector of win probabilities.
#' @details
#'   Note that the \code{"elo.glm.running"} objects will use a model fit on all the data to predict.
#' @examples
#' data(tournament)
#' t1 <- head(tournament, -3)
#' t2 <- tail(tournament, 3)
#' results <- elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = t1,
#'   subset = points.Home != points.Visitor)
#' predict(results)
#' predict(results, newdata = t2)
#' @name predict.elo.glm
NULL
#> NULL

#' @rdname predict.elo.glm
#' @export
predict.elo.glm <- function(object, newdata, type = "response", ...)
{
  if(missing(newdata) || is.null(newdata)) return(stats::predict.glm(object, newdata = NULL, type = type, ...))
  form <- clean_elo_formula(object$elo.terms, drop.neutral = FALSE)
  mf <- elo.model.frame(form, data = newdata, required.vars = c("elos", "neutral"))
  newdata.wide <- mf_to_wide(mf, teams = object$teams)
  stats::predict.glm(object, newdata = newdata.wide, type = type, ...)
}


#' @rdname predict.elo.glm
#' @export
predict.elo.running <- function(object, newdata, running = TRUE, ...)
{
  if((missing(newdata) || is.null(newdata)) && running)
  {
    return(fitted(object, running = TRUE))
  } else if(missing(newdata) || is.null(newdata)) return(fitted(object, running = FALSE))
  NextMethod()
}
