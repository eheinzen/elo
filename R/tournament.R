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
#'     \item{\code{week}}{Week Number}
#'     \item{\code{half}}{The half of the season in which the match was played}
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
  means <- c(18, 12, 17, 13, 16, 15.5, 14, 14.5)
  names(means) <- all.teams

  tournament <- expand.grid(team.Home = all.teams, team.Visitor = all.teams, stringsAsFactors = FALSE)
  tournament <- tournament[tournament$team.Home != tournament$team.Visitor, ]

  tournament$points.Home <- vapply(means[tournament$team.Home] + 3, stats::rpois, 0, n = 1)
  tournament$points.Visitor <- vapply(means[tournament$team.Visitor], stats::rpois, 0, n = 1)

  tournament$week <- 0
  tournament$week[1] <- 1
  for(i in 2:nrow(tournament))
  {
    t1 <- tournament$team.Home[i]
    t2 <- tournament$team.Visitor[i]
    idx <- 1:(i-1)
    wks <- tournament$week[idx]
    tm1 <- tournament$team.Home[idx]
    tm2 <- tournament$team.Visitor[idx]

    tournament$week[i] <- min(setdiff(1:14, wks[tm1 %in% c(t1, t2) | tm2 %in% c(t1, t2)]))
  }

  tournament$half <- ifelse(tournament$week < 8, "First Half of Season", "Second Half of Season")
  tournament <- tournament[order(tournament$week), ]
  rownames(tournament) <- NULL
  attr(tournament, "out.attrs") <- NULL

  tournament
}

