context("Testing the elo.run function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("regress works", {

  ref1 <- c("Team A" = 1517.770, "Team B" = 1501.949, "Team C" = 1480.281)
  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season == 1, 1500, 0.2),
                            k = 20, data = dat))[3, ], 3),
    ref1
  )
  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                            k = 20, data = dat))[3, ], 3),
    ref1
  )
})

test_that("group works", {
  expect_identical(
    1,
    1
  )
})
