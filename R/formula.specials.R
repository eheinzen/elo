
#' Details on \code{elo} formulas
#'
#' @details
#' In the functions in this package, \code{formula} is usually of the form \code{wins.A ~ elo.A + elo.B},
#'   where \code{elo.A} and \code{elo.B} are vectors of Elos, and \code{wins.A} is between 0 and 1,
#'   denoting whether team A (Elo A) won or lost (or something between). \code{elo.prob} also allows
#'   \code{elo.A} and \code{elo.B} to be character or factors, denoting which team(s) played. \code{elo.run}
#'   requires \code{elo.A} to be a vector of teams (sometimes denoted by \code{"team.A"}),
#'   but \code{elo.B} can be either a vector of teams or  else a numeric column
#'   (denoting a fixed-Elo opponent).
#'
#' \code{formula} accepts four special functions in it:
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
#' @name formula.specials
NULL
#> NULL
