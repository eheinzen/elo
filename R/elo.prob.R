#' Elo functions
#'
#' Calculate the probability that team A beats team B. This is vectorized.
#'
#' @inheritParams elo.calc
#' @details
#'   Note that \code{formula} can be missing the \code{wins.A} component. If
#'   present, it's ignored by \code{\link{elo.model.frame}}.
#' @seealso \code{\link{elo.update}}, \code{\link{elo.calc}}
#' @examples
#' elo.prob(1500, 1500)
#' elo.prob(c(1500, 1500), c(1500, 1600))
#'
#' dat <- data.frame(wins.A = c(1, 0), elo.A = c(1500, 1500),
#'                   elo.B = c(1500, 1600), k = c(20, 20))
#' elo.prob(~ elo.A + elo.B, data = dat)
#'
#' ## Also works to include the wins and k:
#' elo.calc(wins.A ~ elo.A + elo.B + k(k), data = dat)
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
elo.prob.default <- function(elo.A, elo.B, ..., adjust.A = 0, adjust.B = 0)
{
  1/(1 + 10^(((elo.B + adjust.B) - (elo.A + adjust.A))/400.0))
}

#' @rdname elo.prob
#' @export
elo.prob.formula <- function(formula, data, na.action, subset, ..., elos = NULL)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo.model.frame)
  Call$required.vars <- "teams"
  mf <- eval(Call, parent.frame())

  t1 <- mf[[1 + has.wins(mf)]]
  t2 <- mf[[2 + has.wins(mf)]]

  if(!is.numeric(t1) || !is.numeric(t2))
  {
    all.teams <- character(0)
    if(!is.numeric(t1))
    {
      t1 <- as.character(t1)
      if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")
      all.teams <- c(all.teams, t1)
    }
    if(!is.numeric(t2))
    {
      t2 <- as.character(t2)
      if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
      all.teams <- c(all.teams, t2)
    }

    all.teams <- sort(unique(all.teams))
    elos <- check_initial_elos(elos, all.teams)

    if(!is.numeric(t1)) t1 <- unname(elos[t1])
    if(!is.numeric(t2)) t2 <- unname(elos[t2])
  }

  elo.prob(t1, t2, ..., adjust.A = mf$`(adj1)`, adjust.B = mf$`(adj2)`)
}
