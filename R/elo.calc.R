
#' Elo functions
#'
#' Calculate post-update Elo values. This is vectorized.
#'
#' @inheritParams elo.model.frame
#' @param elo.A,elo.B Numeric vectors of elo scores.
#' @param wins.A Numeric vector of wins by team A.
#' @param ... Other arguments (not in use at this time).
#' @param adjust.A,adjust.B Numeric vectors to adjust \code{elo.A} and \code{elo.B} by.
#' @seealso \code{\link{elo.prob}}, \code{\link{elo.update}},
#'   \code{elo.model.frame}
#' @return A data.frame with two columns, giving the new Elo values after each update.
#' @examples
#' elo.calc(c(1, 0), c(1500, 1500), c(1500, 1600), k = 20)
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.calc(wins.A ~ elo.A + elo.B + k(k), data = dat)
#' @name elo.calc
NULL
#> NULL

#' @rdname elo.calc
#' @export
elo.calc <- function(wins.A, ...)
{
  UseMethod("elo.calc")
}

#' @rdname elo.calc
#' @export
elo.calc.default <- function(wins.A, elo.A, elo.B, k, ..., adjust.A = 0, adjust.B = 0)
{
  validate_score(wins.A)
  elo.up <- elo.update(wins.A = wins.A, elo.A = elo.A, elo.B = elo.B, k = k, ...,
                       adjust.A = adjust.A, adjust.B = adjust.B)
  if(NCOL(elo.up) == 1) elo.up <- matrix(c(elo.up, elo.up), ncol = 2)
  data.frame(elo.A = elo.A + elo.up[, 1], elo.B = elo.B - elo.up[, 2])
}

#' @rdname elo.calc
#' @export
elo.calc.formula <- function(formula, data, na.action, subset, k = NULL, ...)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k")
  mf <- eval(Call, parent.frame())
  elo.calc(mf$wins.A, mf$elo.A, mf$elo.B, k = mf$k, ...,
           adjust.A = mf$adj.A, adjust.B = mf$adj.B)
}
