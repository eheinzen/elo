
#' \code{elo.glm}
#'
#' Compute a logistic regression model for a matchup.
#'
#' @inheritParams elo.calc
#' @param family,... Arguments passed to \code{\link[stats]{glm}}.
#' @param rm.ties Logical, denoting whether to remove ties on the left-hand side.
#' @return An object of class \code{c("elo.glm", "glm")}.
#' @details
#'   The formula syntax is the same as other \code{elo} functions. A data.frame
#'   of indicator variables is built, where an entry is 1 if a team is home, 0 if
#'   a team didn't play, and -1 if a team is a visitor. A \code{\link{glm}} model is then
#'   run to predict wins.
#'
#'   With this setup, usually one model term will be NA, as the input is linearly dependent.
#'   Consider this team's skill to be zero relative to the other teams.
#'   The intercept represents the home-field advantage.
#' @examples
#' data(tournament)
#' elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament)
#'
#' @seealso \code{\link{score}}, \code{\link{elo.model.frame}}
#' @name elo.glm
NULL
#> NULL

#' @rdname elo.glm
#' @export
elo.glm <- function(formula, data, na.action, subset, family = "binomial", ..., rm.ties = TRUE)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos")
  mf <- eval(Call, parent.frame())
  Terms <- stats::terms(mf)

  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")

  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))

  dat <- lapply(all.teams, function(tm) (rowSums(t1 == tm) > 0) - (rowSums(t2 == tm) > 0))
  names(dat) <- all.teams
  dat$wins.A <- mf$wins.A
  dat <- structure(dat, class = "data.frame", row.names = c(NA_integer_, nrow(mf)))

  if(rm.ties) dat <- dat[dat$wins.A %in% 0:1, , drop = FALSE]

  dat.glm <- stats::glm(wins.A ~ ., data = dat, family = family, ...)

  structure(dat.glm, class = c("elo.glm", class(dat.glm)), rm.ties = rm.ties, all.teams = all.teams)
}
