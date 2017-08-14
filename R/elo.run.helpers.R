
#' as.matrix.elo.run
#'
#' Convert an object from \code{\link{elo.run}} into a matrix of running Elos.
#'
#' @param x An object of class \code{"elo.run"}.
#' @param ... Other arguments (Not in use at this time).
#'
#' @export
as.matrix.elo.run <- function(x, ...)
{
  out <- eloRunAsMatrix(x$elos)
  colnames(out) <- x$teams
  out
}
