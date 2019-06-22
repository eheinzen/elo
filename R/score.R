
#' Create a 1/0/0.5 win "indicator"
#'
#' Create a 1/0/0.5 win "indicator" based on two teams' scores, and test for "score-ness".
#'
#' @param score.A Numeric; the score of the first team (whose wins are to be denoted by 1).
#' @param score.B Numeric; the score of the second team (whose wins are to be denoted by 0).
#' @param x An R object.
#' @return For \code{score}, a vector containing 0, 1, and 0.5 (for ties). For
#'   \code{is.score}, \code{TRUE} or \code{FALSE} depending on whether all values of
#'   \code{x} are between 0 and 1 (inclusive).
#' @seealso \code{\link{score}}
#' @examples
#' score(12, 10)
#' score(10, 10)
#' score(10, 12)
#' @name score
NULL
#> NULL

#' @rdname score
#' @export
score <- function(score.A, score.B)
{
  (score.A > score.B) + 0.5*(score.A == score.B)
}

#' @rdname score
#' @export
is.score <- function(x)
{
  is.numeric(x) && !anyNA(x) && all(0 <= x & x <= 1)
}

validate_score <- function(x)
{
  if(!is.score(x)) stop("The wins should be between 0 and 1 (inclusive).")
  invisible(x)
}
