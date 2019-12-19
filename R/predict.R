
#' Make Predictions on an \code{elo} Object
#'
#' @param object An model from which to get predictions.
#' @param newdata A new dataset containing the same variables as the call
#'   that made \code{object}. If missing, the predicted win probabilities from
#'   \code{object} will be returned.
#' @param regressed See the note on \code{\link{final.elos}}.
#' @param type See \code{\link[stats]{predict.glm}}
#' @param running logical, denoting whether to use the running predicted values. Only makes
#'   sense if \code{newdata} is missing.
#' @param ... Other arguments.
#' @return A vector of win probabilities.
#' @examples
#' data(tournament)
#' t1 <- head(tournament, -3)
#' t2 <- tail(tournament, 3)
#' results <- elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
#'                    data = t1, k = 20)
#' predict(results)
#' predict(results, newdata = t2)
#'
#' results <- elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = t1,
#'   subset = points.Home != points.Visitor)
#' predict(results)
#' predict(results, newdata = t2)
#'
#' results <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = t1,
#'   subset = points.Home != points.Visitor, k = 0.7)
#' predict(results)
#' predict(results, newdata = t2)
#'
#' results <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = t1,
#'   subset = points.Home != points.Visitor)
#' predict(results)
#' predict(results, newdata = t2)
#'
#' results <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = t1,
#'   subset = points.Home != points.Visitor, k = 0.7)
#' predict(results)
#' predict(results, newdata = t2)
#' @details
#'   Note that the \code{"elo.glm.running"} objects will use a model fit on all the data to predict.
#' @name predict.elo
NULL
#> NULL

#' @rdname predict.elo
#' @export
predict.elo.run <- function(object, newdata, ...)
{
  if(missing(newdata) || is.null(newdata)) return(fitted(object))
  form <- clean_elo_formula(stats::terms(object))
  elo.prob(form, data = newdata, ..., elos = final.elos(object))
}


#' @rdname predict.elo
#' @export
predict.elo.run.regressed <- function(object, newdata, regressed = FALSE, ...)
{
  if(missing(newdata) || is.null(newdata)) return(fitted(object))
  form <- clean_elo_formula(stats::terms(object))
  elo.prob(form, data = newdata, ..., elos = final.elos(object, regressed = regressed))
}

#' @rdname predict.elo
#' @export
predict.elo.glm <- function(object, newdata, type = "response", ...)
{
  if(missing(newdata) || is.null(newdata)) return(stats::predict.glm(object, newdata = NULL, type = type, ...))
  form <- clean_elo_formula(object$elo.terms, drop.neutral = FALSE)
  mf <- elo.model.frame(form, data = newdata, required.vars = c("elos", "neutral"))
  newdata.wide <- mf_to_wide(mf, teams = object$teams)
  stats::predict.glm(object, newdata = newdata.wide, type = type, ...)
}


#' @rdname predict.elo
#' @export
predict.elo.running <- function(object, newdata, running = TRUE, ...)
{
  if((missing(newdata) || is.null(newdata)) && running)
  {
    return(fitted(object, running = TRUE))
  } else if(missing(newdata) || is.null(newdata)) return(fitted(object, running = FALSE))
  NextMethod()
}

#' @rdname predict.elo
#' @export
predict.elo.markovchain <- function(object, newdata, ...)
{
  if(missing(newdata) || is.null(newdata)) return(fitted(object))
  form <- clean_elo_formula(object$elo.terms, drop.neutral = FALSE)
  mf <- elo.model.frame(form, data = newdata, required.vars = c("elos", "neutral"))
  if(!is.players(mf$elo.A)) mf$elo.A <- players(mf$elo.A)
  if(!is.players(mf$elo.B)) mf$elo.B <- players(mf$elo.B)

  dat <- data.frame(
    difference = mean_vec_subset_matrix(object$pi, mf$elo.A) - mean_vec_subset_matrix(object$pi, mf$elo.B),
    home.field = mf$home.field, adj.A = mf$adj.A, adj.B = mf$adj.B
  )
  stats::predict.glm(object$fit, newdata = dat, type = "response", ...)
}

#' @rdname predict.elo
#' @export
predict.elo.colley <- function(object, newdata, ...)
{
  if(missing(newdata) || is.null(newdata)) return(fitted(object))
  form <- clean_elo_formula(object$elo.terms, drop.neutral = FALSE)
  mf <- elo.model.frame(form, data = newdata, required.vars = c("elos", "neutral"))
  if(!is.players(mf$elo.A)) mf$elo.A <- players(mf$elo.A)
  if(!is.players(mf$elo.B)) mf$elo.B <- players(mf$elo.B)

  dat <- data.frame(
    difference = mean_vec_subset_matrix(object$pi, mf$elo.A) - mean_vec_subset_matrix(object$pi, mf$elo.B),
    home.field = mf$home.field, adj.A = mf$adj.A, adj.B = mf$adj.B
  )
  stats::predict.glm(object$fit, newdata = dat, type = "response", ...)
}


#' @rdname predict.elo
#' @export
predict.elo.winpct <- function(object, newdata, ...)
{
  if(missing(newdata) || is.null(newdata)) return(fitted(object))
  form <- clean_elo_formula(object$elo.terms, drop.neutral = FALSE)
  mf <- elo.model.frame(form, data = newdata, required.vars = c("elos", "neutral"))
  if(!is.players(mf$elo.A)) mf$elo.A <- players(mf$elo.A)
  if(!is.players(mf$elo.B)) mf$elo.B <- players(mf$elo.B)

  dat <- data.frame(
    difference = mean_vec_subset_matrix(object$win.pct, mf$elo.A) - mean_vec_subset_matrix(object$win.pct, mf$elo.B),
    home.field = mf$home.field, adj.A = mf$adj.A, adj.B = mf$adj.B
  )
  stats::predict.glm(object$fit, newdata = dat, type = "response", ...)
}
