context("Testing the elo.run function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("Basic Elo calculations work", {

  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat), 3),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B, k = 20, data = dat), 3),
    c("Team A" = 1519.712, "Team B" = 1500.008, "Team C" = 1480.279)
  )

  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat), 3),
    rnd.fin(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat)),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B, k = 20, data = dat), 3),
    rnd.fin(elo.run2(wins.A ~ team.A + team.B, k = 20, data = dat))
  )

  expect_eq(
    rnd.fin(elo.run(wins.A ~ team.A + dummy.B + k(k.column), data = dat)),
    rnd.fin(elo.run2(wins.A ~ team.A + dummy.B + k(k.column), data = dat)),
    c("Team A" = 1519.712, "Team C" = 1490)
  )

  expect_error(elo.run(wins.A ~ dummy.B + team.B, k = 20, data = dat))
  expect_error(elo.run(wins.A ~ dummy.A + dummy.B, k = 20, data = dat))
  expect_error(elo.run2(wins.A ~ dummy.B + team.B, k = 20, data = dat))
  expect_error(elo.run2(wins.A ~ dummy.A + dummy.B, k = 20, data = dat))

  expect_identical(
    elo.calc(dat$wins.A, dat$dummy.A, dat$dummy.B, k = 20),
    data.frame(elo.A = c(1510, 1510, 1490), elo.B = c(1490, 1490, 1510))
  )
})

test_that("'k' specification works either as vector or constant", {
  expect_eq(
    elo.run(wins.A ~ team.A + team.B + k(k.column), data = dat)$elos,
    elo.run(wins.A ~ team.A + team.B, k = 20, data = dat)$elos,
    elo.run2(wins.A ~ team.A + team.B + k(k.column), data = dat)$elos,
    elo.run2(wins.A ~ team.A + team.B, k = 20, data = dat)$elos
  )
  expect_eq(
    elo.run(wins.A ~ team.A + team.B + k(k.column), data = dat)$elos,
    elo.run(wins.A ~ team.A + team.B, k = dat$k.column, data = dat)$elos,
    elo.run2(wins.A ~ team.A + team.B + k(k.column), data = dat)$elos,
    elo.run2(wins.A ~ team.A + team.B, k = dat$k.column, data = dat)$elos
  )
  expect_error(elo.run(wins.A ~ team.A + team.B, k = c(20, 20), data = dat), "must be length 1 or")
  expect_error(elo.run2(wins.A ~ team.A + team.B, k = c(20, 20), data = dat), "must be length 1 or")
})

test_that("'adjust' specification works either as a vector or constant", {
  expect_eq(
    elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = dat, k = 20)$elos,
    elo.run(wins.A ~ adjust(team.A, home.field) + team.B, data = dat, k = 20)$elos,
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B, data = dat, k = 20)$elos,
    elo.run2(wins.A ~ adjust(team.A, home.field) + team.B, data = dat, k = 20)$elos
  )
})

results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = dat, k = 20)
results2 <- elo.run2(wins.A ~ adjust(team.A, 10) + team.B, data = dat, k = 20)

test_that("prediction works correctly", {
  newdat <- data.frame(team.A = "Team A", team.B = "Team B")
  expect_eq(
    predict(results, newdata = newdat),
    elo.prob(final.elos(results)["Team A"], final.elos(results)["Team B"], adjust.A = 10),
    predict(results2, newdata = newdat),
    elo.prob(final.elos(results2)["Team A"], final.elos(results2)["Team B"], adjust.A = 10)
  )
  expect_equal(length(predict(results)), nrow(dat))
  expect_equal(length(predict(results2)), nrow(dat))
})

test_that("Deep copying (#25)", {
  expect_equal(results$initial.elos, c("Team A" = 1500, "Team B" = 1500, "Team C" = 1500))
  expect_equal(unname(as.matrix(results)[1, "Team C"]), 1500)
})

test_that("Multiple k's (#45)", {
  expect_eq(
    elo.run(wins.A ~ adjust(team.A, 10) + team.B + k(k.column), data = dat)$elos,
    elo.run(wins.A ~ adjust(team.A, 10) + team.B + k(k.column, k.column), data = dat)$elos,
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B + k(k.column), data = dat)$elos,
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B + k(k.column, k.column), data = dat)$elos
  )

  results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B + k(k.column, 2*k.column), data = dat)
  results2 <- elo.run2(wins.A ~ adjust(team.A, 10) + team.B + k(k.column, 2*k.column), data = dat)
  expect_eq(
    rnd.mat(results, 3),
    rnd.mat(results2, 3),
    c("Team A" = 1519.145, "Team B" = 1501.183, "Team C" = 1470.830)
  )
})

test_that("Custom updates (#47)", {
  custom_fun <- function(wins.A, elo.A, elo.B, k, adjust.A, adjust.B, ...)
  {
    wins.A - elo.prob(elo.A, elo.B, adjust.A = adjust.A, adjust.B = adjust.B)
  }
  expect_eq(
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B + k(k.column), data = dat, update.fun = custom_fun)$elos,
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B, k = 20, data = dat, update.fun = custom_fun)$elos,
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B, k = 1, data = dat)$elos
  )
  custom_fun2 <- function(wins.A, elo.A, elo.B, k, adjust.A, adjust.B, ...)
  {
    k*(wins.A - elo.prob(elo.A, elo.B, adjust.A = ifelse(elo.A > 1500, adjust.A / 2, adjust.A), adjust.B = adjust.B))
  }
  expect_identical(
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B, k = 20, data = dat, update.fun = custom_fun2)$elos[, -3],
    elo.run2(wins.A ~ adjust(team.A, c(10, 5, 10)) + team.B, k = 20, data = dat)$elos[, -3]
  )
  custom_prob <- function(elo.A, elo.B, adjust.A, adjust.B)
  {
    unname(1/(1 + 10^(((elo.B + adjust.B) - (elo.A + ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))/400.0)))
  }
  expect_identical(
    elo.run2(wins.A ~ adjust(team.A, 10) + team.B, k = 20, data = dat, prob.fun = custom_prob, update.fun = custom_fun2)$elos,
    elo.run2(wins.A ~ adjust(team.A, c(10, 5, 10)) + team.B, k = 20, data = dat)$elos
  )
})

