
dat <- data.frame(
  team.A = c("Team A", "Team A", "Team C"),
  team.B = c("Team B", "Team C", "Team B"),
  wins.A = c(1, 1, 0),
  dummy.A = 1500,
  dummy.B = 1500,
  k.column = 20,
  home.field = 10,
  season = c(1, 2, 2),
  week = c(1, 1, 2),
  p1.A = c("Player 1", "Player 2", "Player 3"),
  p2.A = c("Player 2", "Player 3", "Player 1"),
  p1.B = c("Player 4", "Player 4", "Player 4"),
  p2.B = c("Player 5", "Player 5", "Player 6")
)

init <- c("Team A" = 1600, "Team B" = 1500, "Team C" = 1400)
init.ply <- c("Player 1" = 750, "Player 2" = 750, "Player 3" = 750,
              "Player 4" = 600, "Player 5" = 900, "Player 6" = 750)
init.ply2 <- c("Player 1" = 750, "Player 2" = 700, "Player 3" = 650,
               "Player 4" = 600, "Player 5" = 900, "Player 6" = 750)
rnd.mat <- function(x, i) round(as.matrix(x)[i, ], 3)
rnd.fin <- function(x, ...) round(final.elos(x, ...), 3)
expect_eq <- function(x, y, z, w = NULL)
{
  expect_equal(x, y)
  expect_equal(x, z)
  if(!is.null(w)) expect_equal(x, w)
}
