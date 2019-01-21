
#' Details on \code{elo} formulas and the specials therein
#'
#' Details on \code{elo} functions and the special functions allowed in them to change functions' behaviors.
#'
#' @param x A vector.
#' @param adjustment A single value or a vector of the same length as \code{x}: how much to adjust the Elos in \code{x}.
#' @param to Numeric: what Elo to regress to. Can be a single value or named vector the same length
#'   as the number of teams.
#' @param by Numeric: by how much should Elos be regressed toward \code{to}.
#' @param regress.unused Logical: whether to continue regressing teams which have stopped playing.
#' @param ... Vectors to be coerced to character, which comprise of the players of a team.
#' @param weights A vector giving the weights of Elo updates for the players in \code{...}.
#' @details
#' In the functions in this package, \code{formula} is usually of the form \code{wins.A ~ elo.A + elo.B},
#'   where \code{elo.A} and \code{elo.B} are vectors of Elos, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A (Elo A) won or lost (or something between). \code{elo.prob} also allows
#'   \code{elo.A} and \code{elo.B} to be character or factors, denoting which team(s) played. \code{elo.run}
#'   requires \code{elo.A} to be a vector of teams or a players matrix from \code{players()}
#'   (sometimes denoted by \code{"team.A"}), but \code{elo.B} can be either a vector of teams or
#'   players matrix (\code{"team.B"}) or else a numeric column (denoting a fixed-Elo opponent).
#'   \code{elo.glm} requires both to be a vector of teams or players matrix.
#'
#' \code{formula} accepts five special functions in it:
#'
#' \code{k()} allows for complicated Elo updates. For
#'   constant Elo updates, use the \code{k = } argument instead of this special function.
#'
#' \code{adjust()} allows for Elos to be adjusted for, e.g., home-field advantage. The second argument
#'   to this function can be a scalar or vector of appropriate length.
#'
#' \code{regress()} can be used to regress Elos back to a fixed value
#'   after certain matches. Giving a logical vector identifies these matches after which to
#'   regress back to the mean. Giving any other kind of vector regresses after the appropriate
#'   groupings (see, e.g., \code{\link{duplicated}(..., fromLast = TRUE)}). The other three arguments determine
#'   what Elo to regress to (\code{to = }), by how much to regress toward that value
#'   (\code{by = }), and whether to continue regressing teams which have stopped playing (\code{regress.unused},
#'   default = \code{TRUE}).
#'
#' \code{group()} is used to group matches (by, e.g., week). It is fed to \code{\link{as.matrix.elo.run}}
#'   to produce only certain rows of matrix output.
#'
#' \code{players()} is used for multiple players on a team contributing to an overall Elo. The Elo updates
#'   are then assigned based on the specified weights.
#' @name formula.specials
NULL
#> NULL

#' @rdname formula.specials
#' @export
k <- function(x) structure(x, class = c("elo.k", class(x)))

#' @rdname formula.specials
#' @export
adjust <- function(x, adjustment) {
  if(!(length(adjustment) %in% c(1, length(x))))
    stop("The second argument to 'adjust()' needs to be length 1 or the same length as the first argument.")

  attr(x, "adjust") <- if(length(adjustment) == 1) rep(adjustment, times = length(x)) else adjustment
  class(x) <- c("elo.adjust", class(x))
  x
}


#' @export
"[.elo.adjust" <- function(x, i, j, drop = FALSE)
{
  out <- NextMethod()
  adjust(out, attr(x, "adjust")[i])
}

remove_elo_adjust <- function(x)
{
  class(x) <- class(x)[!(class(x) %in% "elo.adjust")]
  attr(x, "adjust") <- NULL
  x
}

#' @rdname formula.specials
#' @export
regress <- function(x, to, by, regress.unused = TRUE) {
  if(!is.numeric(to) || anyNA(to)) stop("regress: 'to' must be numeric.")
  if(!is.numeric(by) || length(by) != 1 || anyNA(by) || by > 1 || by < 0)
    stop("regress: 'by' must be 0 <= by <= 1")
  if(!is.logical(regress.unused) || length(regress.unused) != 1 || anyNA(regress.unused))
    stop("regress: 'regress.unused' must be a single logical value.")
  attr(x, "to") <- to
  attr(x, "by") <- by
  attr(x, "regress.unused") <- regress.unused
  class(x) <- c("elo.regress", class(x))
  x
}

#' @export
"[.elo.regress" <- function(x, i)
{
  out <- NextMethod()
  regress(out, attr(x, "to"), attr(x, "by"), attr(x, "regress.unused"))
}

#' @rdname formula.specials
#' @export
group <- function(x) structure(x, class = c("elo.group", class(x)))
