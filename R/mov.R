
#' Create a "margin of victory" column
#'
#' Create a "margin of victory" based on two teams' scores
#'
#' @param score.A Numeric; the score of the first team. Alternatively, this can
#'   be a pre-computed margin of victory which will get compared to 0.
#' @param score.B Numeric; the score of the second team; default is 0, in case
#'   \code{score.A} is already a margin of victory..
#' @return An object with class \code{"elo.mov"}, denoting \code{score.A} = \code{score.B}.
#' @seealso \code{\link{score}}
#' @examples
#' mov(12, 10)
#' mov(10, 10)
#' mov(10, 12)
#' @name elo.mov
NULL
#> NULL

#' @rdname elo.mov
#' @export
mov <- function(score.A, score.B = 0)
{
  out <- score.A - score.B
  structure(out, class = c("elo.mov", class(out)[class(out) != "elo.mov"]))
}

#' @export
`[.elo.mov` <- function(x, i)
{
  out <- NextMethod()
  mov(out)
}
