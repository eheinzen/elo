

context("Testing the elo.calc function")

df <- data.frame(
  team.A = c("Team A", "Team A", "Team C"),
  team.B = c("Team B", "Team C", "Team B"),
  wins.A = c(1, 1, 0),
  dummy.A = 1500,
  dummy.B = 1500,
  k.column = 20,
  home.field = 10
)

init <- c("Team A" = 1600, "Team B" = 1500, "Team C" = 1400)

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("Basic Elo calculations work", {

  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = df))[4, ], 3),
    c("Team A" = 1519.712, "Team B" = 1500.008, "Team C" = 1480.279)
  )

  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = df))[4, ], 3),
    round(last(elo.run(wins.A ~ team.A + team.B, k = 20, data = df)), 3)
  )

  expect_identical(
    round(last(elo.run(wins.A ~ team.A + dummy.B + k(k.column), data = df)), 3),
    c("Team A" = 1519.712, "Team C" = 1490)
  )

  expect_identical(
    round(last(elo.run(wins.A ~ dummy.B + team.B, k = 20, data = df)), 3),
    c("Team B" = 1500.288, "Team C" = 1490)
  )

  expect_warning(elo.run(wins.A ~ dummy.A + dummy.B, k = 20, data = df))

  expect_identical(
    suppressWarnings(elo.run(wins.A ~ dummy.A + dummy.B, k = 20, data = df)),
    data.frame(elo.A = c(1510, 1510, 1490), elo.B = c(1490, 1490, 1510))
  )
})

test_that("'k' specification works either as vector or constant", {
  expect_identical(
    elo.run(wins.A ~ team.A + team.B + k(k.column), data = df)$elos,
    elo.run(wins.A ~ team.A + team.B, k = 20, data = df)$elos
  )
})

test_that("'adjust' specification works either as a vector or constant", {
  expect_identical(
    elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = df, k = 20)$elos,
    elo.run(wins.A ~ adjust(team.A, home.field) + team.B, data = df, k = 20)$elos
  )

})
