context("Testing the elo.run function for group() and regress()")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("regress works()", {

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
  expect_identical(
    round(final.elos(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                             k = 20, data = dat), regressed = FALSE), 3),
    ref1
  )
  expect_identical(
    round(as.vector(final.elos(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                                       k = 20, data = dat), regressed = TRUE)), 3),
    round(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                  k = 20, data = dat)$elos.regressed[2, ], 3)
  )
  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                            k = 20, data = dat))[2, ], 3),
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                            k = 20, data = dat, subset = week < 2))[2, ], 3)
  )
  expect_identical(
    round(final.elos(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                             k = 20, data = dat,
                             initial.elos = c("Team A" = 1600, "Team B" = 1500, "Team C" = 1400)),
                     regressed = FALSE), 3),
    c("Team A" = 1590.870, "Team B" = 1501.457, "Team C" = 1387.673)
  )
})

test_that("group works()", {
  expect_identical(
    as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat))[2:3, ],
    as.matrix(elo.run(wins.A ~ team.A + team.B + group(week), k = 20, data = dat))
  )
  expect_identical(
    as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat))[2:3, ],
    as.matrix(elo.run(wins.A ~ team.A + team.B + group(c(FALSE, TRUE, TRUE)), k = 20, data = dat))
  )
})

test_that("'group()' and 'regress()' both work", {
  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                            k = 20, data = dat))[2:3, ], 3),
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2) + group(week),
                            k = 20, data = dat)), 3)
  )
})

test_that("'formula' can be in any order", {
  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2),
                            k = 20, data = dat))[2:3, ], 3),
    round(as.matrix(elo.run(wins.A ~ k(k.column) + team.A + regress(season, 1500, 0.2) +
                              group(week) + team.B, data = dat)), 3)
  )
})


test_that("prediction works correctly", {
  results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B + regress(season, 1500, 0.2), data = dat, k = 20)
  newdat <- data.frame(team.A = "Team A", team.B = "Team B")
  expect_identical(
    predict(results, newdata = newdat),
    elo.prob(final.elos(results)["Team A"], final.elos(results)["Team B"], adjust.A = 10)
  )
  expect_equal(length(predict(results)), nrow(dat))
})
