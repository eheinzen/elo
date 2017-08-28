
#' Elo functions
#'
#' Calculate post-update Elo values. This is vectorized.
#'
#' @inheritParams elo.model.frame
#' @param elo.A,elo.B Numeric vectors of elo scores.
#' @param wins.A Numeric vector of wins by team A.
#' @param ... Other arguments (not in use at this time).
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.update}}
#' @examples
#' elo.calc(c(1500, 1500), c(1500, 1600), c(1, 0), k = 20)
#' @name elo.calc
NULL
#> NULL

#' @rdname elo.calc
#' @export
elo.calc <- function(elo.A, ...)
{
  UseMethod("elo.calc")
}

#' @rdname elo.calc
#' @export
elo.calc.default <- function(elo.A, elo.B, wins.A, k, ...)
{
  elo.up <- elo.update(elo.A = elo.A, elo.B = elo.B, wins.A = wins.A, k = k)
  data.frame(elo.A = elo.A + elo.up, elo.B = elo.B - elo.up)
}

#' @rdname elo.calc
#' @export
elo.calc.formula <- function(formula, data, na.action, subset, k = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$envir <- parent.frame()
  mf <- eval(Call, parent.frame())
}
