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


make_tournament_dataset <- function(seed = NULL)
{
  set.seed(seed)

  all.teams <- c("Athletic Armadillos", "Blundering Baboons", "Cunning Cats", "Defense-less Dogs", "Elegant Emus",
                 "Fabulous Frogs", "Gallivanting Gorillas", "Helpless Hyenas")
  true.elo <- c(1800, 1200, 1700, 1300, 1600, 1550, 1400, 1450)
  names(true.elo) <- all.teams

  tournament <- expand.grid(team.Home = all.teams, team.Visitor = all.teams)
  tournament <- tournament[tournament$team.Home != tournament$team.Visitor, ]

  tournament$elo.Home <- true.elo[tournament$team.Home]
  tournament$elo.Visitor <- true.elo[tournament$team.Visitor]

  tournament$adjust.Home <- 200

  tournament$p.Home <- elo.prob(tournament$elo.Home + tournament$adjust.Home, tournament$elo.Visitor)
  tournament$wins.Home <- stats::runif(nrow(tournament)) < tournament$p.Home

  tournament$elo.Diff <- tournament$elo.Home + tournament$adjust.Home - tournament$elo.Visitor

  tournament$points.Home <- stats::rpois(nrow(tournament), lambda = 10)
  tournament$points.Visitor <- tournament$points.Home +
    ifelse(tournament$elo.Diff >= 0 & tournament$wins.Home >  0, pmin(-floor(tournament$elo.Diff / 100), -1), # home team was supposed to win and did
    ifelse(tournament$elo.Diff >= 0 & tournament$wins.Home == 0, pmax(2 - floor(tournament$elo.Diff / 100),  1), # home team was supposed to win but didn't
    ifelse(tournament$elo.Diff <  0 & tournament$wins.Home >  0, pmin(2 + floor(tournament$elo.Diff / 100), -1), # visiting team was supposed to win but didn't
    ifelse(tournament$elo.Diff <  0 & tournament$wins.Home == 0, pmax(-floor(tournament$elo.Diff / 100),  1), NA # visiting team was supposed to win and did
  ))))

  stopifnot(isSymmetric(matrix(table(tournament$wins.Home, tournament$points.Home > tournament$points.Visitor), nrow = 2)))
  tournament$elo.Home <- NULL
  tournament$elo.Visitor <- NULL
  tournament$elo.Diff <- NULL
  tournament$p.Home <- NULL
  tournament$wins.Home <- NULL
  tournament$adjust.Home <- NULL

  rownames(tournament) <- NULL
  attr(tournament, "out.attrs") <- NULL

  tournament
}

