
dat <- data.frame(
  team.A = c("Team A", "Team A", "Team C"),
  team.B = c("Team B", "Team C", "Team B"),
  wins.A = c(1, 1, 0),
  dummy.A = 1500,
  dummy.B = 1500,
  k.column = 20,
  home.field = 10,
  season = c(1, 2, 2),
  week = c(1, 1, 2)
)

init <- c("Team A" = 1600, "Team B" = 1500, "Team C" = 1400)
