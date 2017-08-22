#' Elo functions
#'
#' Calculate the probability that team A beats team B. This is vectorized.
#'
#' @inheritParams elo.model.frame
#' @inheritParams elo.calc
#' @seealso \code{\link{elo.update}}, \code{\link{elo.calc}}
#' @examples
#' elo.prob(1500, 1500)
#' elo.prob(c(1500, 1500), c(1500, 1600))
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
elo.prob.default <- function(elo.A, elo.B, ...)
{
  1/(1 + 10^((elo.B - elo.A)/400.0))
}

#' @rdname elo.prob
#' @export
elo.prob.formula <- function(formula, data, na.action, subset, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$envir <- parent.frame()
  mf <- eval(Call, parent.frame())


}
