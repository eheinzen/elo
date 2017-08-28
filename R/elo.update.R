
#' Elo functions
#'
#' Calculate the update value for a given Elo matchup. This is used in
#' \code{\link{elo.calc}}, which reports the post-update Elo values. This is vectorized.
#'
#' @inheritParams elo.calc
#' @examples
#' elo.update(c(1500, 1500), c(1500, 1600), c(1, 0), k = 20)
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.calc}}
#' @name elo.update
NULL
#> NULL

#' @rdname elo.update
#' @export
elo.update <- function(wins.A, ...)
{
  UseMethod("elo.update")
}

#' @rdname elo.update
#' @export
elo.update.default <- function(wins.A, elo.A, elo.B, k, ...)
{
  validate_score(wins.A)
  k*(wins.A - elo.prob(elo.A, elo.B))
}

#' @rdname elo.update
#' @export
elo.update.formula <- function(formula, data, na.action, subset, k = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$required.vars <- c("wins", "teams", "k")
  mf <- eval(Call, parent.frame())
  elo.update(mf[[1]], mf[[2]] + mf$`(adj1)`, mf[[3]] + mf$`(adj2)`, k = mf[[4]], ...)
}

