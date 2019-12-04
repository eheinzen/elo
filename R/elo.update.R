
#' Elo functions
#'
#' Calculate the update value for a given Elo matchup. This is used in
#' \code{\link{elo.calc}}, which reports the post-update Elo values. This is vectorized.
#'
#' @inheritParams elo.calc
#' @return A vector of Elo updates.
#' @examples
#' elo.update(c(1, 0), c(1500, 1500), c(1500, 1600), k = 20)
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.update(wins.A ~ elo.A + elo.B + k(k), data = dat)
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.calc}},
#'   \code{elo.model.frame}
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
elo.update.default <- function(wins.A, elo.A, elo.B, k, ..., adjust.A = 0, adjust.B = 0)
{
  validate_score(wins.A)
  remove_elo_k(k)*(wins.A - elo.prob(elo.A, elo.B, ..., adjust.A = adjust.A, adjust.B = adjust.B))
}

#' @rdname elo.update
#' @export
elo.update.formula <- function(formula, data, na.action, subset, k = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k")
  mf <- eval(Call, parent.frame())
  elo.update(mf$wins.A, mf$elo.A, mf$elo.B, k = mf$k, ...,
             adjust.A = mf$adj.A, adjust.B = mf$adj.B)
}

