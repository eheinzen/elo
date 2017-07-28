
#' Elo functions
#'
#' \code{elo.prob} calculates the probability that team A beats team B. This is vectorized.
#'
#' @param elo.A,elo.B Numeric vectors of elo scores.
#' @param wins.A Numeric vector of wins by team A.
#' @param k Numeric vector (or scalar) of k-values.
#' @details
#'   Originally, I was going to script these in Rcpp, but the performance benefits are only realized
#'   when the vectors get to be ~1000 elements long.
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
