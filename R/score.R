
#' Create a 1/0/0.5 win "indicator"
#'
#' Create a 1/0/0.5 win "indicator" based on two teams' scores.
#'
#' @param score.A Numeric; the score of the first team (whose wins are to be denoted by 1).
#' @param score.B Numeric; the score of the second team (whose wins are to be denoted by 0).
#' @return A vector containing 0, 1, and 0.5 (for ties).
#' @examples
#' score(12, 10)
#' score(10, 10)
#' score(10, 12)
#' @export
score <- function(score.A, score.B)
{
  (score.A > score.B) + 0.5*(score.A == score.B)
}
