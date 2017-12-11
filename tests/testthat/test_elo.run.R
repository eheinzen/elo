context("Testing the elo.run function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("Basic Elo calculations work", {

  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat))[3, ], 3),
    c("Team A" = 1519.712, "Team B" = 1500.008, "Team C" = 1480.279)
  )

  expect_identical(
    round(as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat))[3, ], 3),
    round(final.elos(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat)), 3)
  )

  expect_identical(
    round(final.elos(elo.run(wins.A ~ team.A + dummy.B + k(k.column), data = dat)), 3),
    c("Team A" = 1519.712, "Team C" = 1490)
  )

  expect_error(elo.run(wins.A ~ dummy.B + team.B, k = 20, data = dat))
  expect_error(elo.run(wins.A ~ dummy.A + dummy.B, k = 20, data = dat))

  expect_identical(
    elo.calc(dat$wins.A, dat$dummy.A, dat$dummy.B, k = 20),
    data.frame(elo.A = c(1510, 1510, 1490), elo.B = c(1490, 1490, 1510))
  )
})

test_that("'k' specification works either as vector or constant", {
  expect_identical(
    elo.run(wins.A ~ team.A + team.B + k(k.column), data = dat)$elos,
    elo.run(wins.A ~ team.A + team.B, k = 20, data = dat)$elos
  )
})

test_that("'adjust' specification works either as a vector or constant", {
  expect_identical(
    elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = dat, k = 20)$elos,
    elo.run(wins.A ~ adjust(team.A, home.field) + team.B, data = dat, k = 20)$elos
  )
})

results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = dat, k = 20)
test_that("prediction works correctly", {
  newdat <- data.frame(team.A = "Team A", team.B = "Team B")
  expect_identical(
    predict(results, newdata = newdat),
    elo.prob(final.elos(results)["Team A"], final.elos(results)["Team B"], adjust.A = 10)
  )
  expect_equal(length(predict(results)), nrow(dat))
})

test_that("#25: Deep copying", {
  expect_equal(results$initial.elos, c("Team A" = 1500, "Team B" = 1500, "Team C" = 1500))
  expect_equal(unname(as.matrix(results)[1, "Team C"]), 1500)
})
