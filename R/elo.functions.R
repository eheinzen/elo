
#' Elo functions
#'
#' \code{elo.prob} calculates the probability that team A beats team B.
#' \code{elo.update} calculates the update value for a given Elo matchup, and is used in
#' \code{elo.calc}, which reports the post-update Elo values.
#'
#' These are all vectorized.
#'
#' @param elo.A,elo.B Numeric vectors of elo scores.
#' @param wins.A Numeric vector of wins by team A.
#' @param k Numeric vector or scalar of k-values.
#' @details
#'   Originally, I was going to script these in Rcpp, but the performance benefits are only realized
#'   when the vectors get to be ~1000 elements long.
#' @examples
#' elo.prob(1500, 1500)
#' elo.prob(c(1500, 1500), c(1500, 1600))
#'
#' elo.update(c(1500, 1500), c(1500, 1600), c(1, 0), k = 20)
#' elo.calc(c(1500, 1500), c(1500, 1600), c(1, 0), k = 20)
#' @name elo.functions
NULL
#> NULL

#' @rdname elo.functions
#' @export
elo.prob <- function(elo.A, elo.B)
{
  1/(1 + 10^((elo.B - elo.A)/400.0))
}

#' @rdname elo.functions
#' @export
elo.update <- function(elo.A, elo.B, wins.A, k)
{
  k*(wins.A - elo.prob(elo.A, elo.B))
}

#' @rdname elo.functions
#' @export
elo.calc <- function(elo.A, elo.B, wins.A, k)
{
  elo.up <- elo.update(elo.A = elo.A, elo.B = elo.B, wins.A = wins.A, k = k)
  data.frame(elo.A = elo.A + elo.up, elo.B = elo.B - elo.up)
}
