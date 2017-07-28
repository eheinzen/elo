#' \code{tournament}: Mock data for examples
#'
#' A fake dataset containing results from "animal-ball" matches.
#'
#' @format A data frame with 56 observations on the following 4 variables:
#'   \describe{
#'     \item{\code{team.Home}}{The home team for the match}
#'     \item{\code{team.Visitor}}{The visiting team for the match}
#'     \item{\code{points.Home}}{Number of points scored by the home team}
#'     \item{\code{points.Visitor}}{Number of points scored by the visiting team}
#'   }
#' @examples
#' data(tournament)
#' str(tournament)
#' @name tournament
NULL
#> NULL

# tournament <- make_tournament_dataset(88)
# save(tournament, file = "data/tournament.RData")
