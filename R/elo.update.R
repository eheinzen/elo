
#' Elo functions
#'
#' Calculate the update value for a given Elo matchup. This is used in
#' \code{\link{elo.calc}}, which reports the post-update Elo values.
#'
#' These are all vectorized.
#'
#' @inheritParams elo.model.frame
#' @inheritParams elo.calc
#' @examples
#' elo.update(c(1500, 1500), c(1500, 1600), c(1, 0), k = 20)
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.calc}}
#' @name elo.update
NULL
#> NULL

#' @rdname elo.update
#' @export
elo.update <- function(elo.A, ...)
{
  UseMethod("elo.update")
}

#' @rdname elo.update
#' @export
elo.update.default <- function(elo.A, elo.B, wins.A, k, ...)
{
  k*(wins.A - elo.prob(elo.A, elo.B))
}

#' @rdname elo.update
#' @export
elo.update.formula <- function(formula, data, na.action, subset, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$envir <- parent.frame()
  mf <- eval(Call, parent.frame())

}

