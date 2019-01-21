#' Elo functions
#'
#' Calculate the probability that team A beats team B. This is vectorized.
#'
#' @inheritParams elo.calc
#' @param elo.A,elo.B Numeric vectors of elo scores, or else vectors of teams.
#' @param elos An optional named vector containing Elo ratings for all teams in \code{formula}
#'   or \code{elo.A} and \code{elo.B}.
#' @return A vector of Elo probabilities.
#' @details
#'   Note that \code{formula} can be missing the \code{wins.A} component. If
#'   present, it's ignored by \code{\link{elo.model.frame}}.
#' @seealso \code{\link{elo.update}}, \code{\link{elo.calc}},
#'   \code{elo.model.frame}
#' @examples
#' elo.prob(1500, 1500)
#' elo.prob(c(1500, 1500), c(1500, 1600))
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.prob(~ elo.A + elo.B, data = dat)
#'
#' ## Also works to include the wins and k:
#' elo.prob(wins.A ~ elo.A + elo.B + k(k), data = dat)
#'
#' ## Also allows teams
#' elo.prob(c("A", "B"), c("C", "C"), elos = c(A = 1500, B = 1600, C = 1500))
#'
#' @name elo.prob
NULL
#> NULL

#' @rdname elo.prob
#' @export
elo.prob <- function(elo.A, ...)
{
  UseMethod("elo.prob")
}

#' @rdname elo.prob
#' @export
elo.prob.default <- function(elo.A, elo.B, ..., elos = NULL, adjust.A = 0, adjust.B = 0)
{
  if(!is.numeric(elo.A) || !is.numeric(elo.B))
  {
    all.teams <- character(0)
    if(!is.numeric(elo.A))
    {
      if(!is.players(elo.A)) elo.A <- players(elo.A)
      if(anyNA(elo.A)) stop("NAs were found in elo.A; check that it can be coerced to character.")
      all.teams <- as.character(elo.A)
    }
    if(!is.numeric(elo.B))
    {
      if(!is.players(elo.B)) elo.B <- players(elo.B)
      if(anyNA(elo.B)) stop("NAs were found in elo.B; check that it can be coerced to character.")
      all.teams <- c(all.teams, as.character(elo.B))
    }

    all.teams <- sort(unique(all.teams))
    elos <- check_named_elos(elos, all.teams)

    if(!is.numeric(elo.A)) elo.A <- rowSums(matrix(elos[elo.A], nrow = nrow(elo.A)))
    if(!is.numeric(elo.B)) elo.B <- rowSums(matrix(elos[elo.B], nrow = nrow(elo.B)))
  }

  unname(1/(1 + 10^(((elo.B + adjust.B) - (elo.A + adjust.A))/400.0)))
}

#' @rdname elo.prob
#' @export
elo.prob.formula <- function(formula, data, na.action, subset, ..., elos = NULL)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  mf <- eval(Call, parent.frame())

  elo.prob(mf$elo.A, mf$elo.B, ..., adjust.A = mf$adj.A, adjust.B = mf$adj.B, elos = elos)
}
