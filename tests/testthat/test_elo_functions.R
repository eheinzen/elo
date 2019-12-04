context("Testing the other elo functions")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("elo.prob works", {
  expect_equal(elo.prob(1500, 1500), 0.5)
  expect_equal(
    elo.prob(dat$dummy.A, dat$dummy.B),
    elo.prob(~ dummy.A + dummy.B, data = dat)
  )
  expect_equal(
    elo.prob(dat$dummy.A, dat$dummy.B, adjust.A = 10, adjust.B = 20),
    elo.prob(~ adjust(dummy.A, 10) + adjust(dummy.B, 20), data = dat)
  )
  expect_equal(
    elo.prob(dat$dummy.A, dat$dummy.B, adjust.A = 10, adjust.B = 20),
    elo.prob(dat$dummy.A + 10, dat$dummy.B + 20)
  )
  expect_equal(
    elo.prob(~ dummy.A + dummy.B, data = dat),
    elo.prob(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
  )

  #### with teams ####

  expect_equal(
    elo.prob(dat$team.A, dat$team.B, elos = init),
    elo.prob(~ team.A + team.B, data = dat, elos = init)
  )
  expect_equal(
    elo.prob(c(1600, 1600, 1400), c(1500, 1400, 1500)),
    elo.prob(~ team.A + team.B, data = dat, elos = init)
  )
  expect_equal(
    elo.prob(c(1600, 1600, 1400), c(1500, 1500, 1500)),
    elo.prob(~ team.A + dummy.B, data = dat, elos = init)
  )
  expect_equal(
    elo.prob(c(1600, 1600, 1400), c(1500, 1500, 1500), adjust.A = 20),
    elo.prob(~ adjust(team.A, 20) + dummy.B, data = dat, elos = init)
  )
  expect_error(
    elo.prob(~ team.A + team.B, na.action = na.pass,
             data = data.frame(team.A = c("A", NA), team.B = 1500, stringsAsFactors = FALSE))
  )
})

test_that("elo.update works", {
  expect_equal(elo.update(1, 1500, 1500, k = 20), 10)
  expect_equal(
    elo.update(dat$wins.A, dat$dummy.A, dat$dummy.B, k = dat$k.column),
    elo.update(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
  )
  expect_equal(
    elo.update(dat$wins.A, dat$dummy.A, dat$dummy.B, k = dat$k.column, adjust.A = 10, adjust.B = 20),
    elo.update(wins.A ~ adjust(dummy.A, 10) + adjust(dummy.B, 20) + k(k.column), data = dat)
  )
  expect_equal(
    elo.update(dat$wins.A, dat$dummy.A, dat$dummy.B, k = dat$k.column, adjust.A = 10, adjust.B = 20),
    elo.update(dat$wins.A, dat$dummy.A + 10, dat$dummy.B + 20, k = dat$k.column)
  )
  expect_equal(
    elo.update(wins.A ~ dummy.A + dummy.B, data = dat, k = 20),
    elo.update(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
  )
})

test_that("elo.calc works", {
  expect_equal(elo.calc(1, 1500, 1500, k = 20), data.frame(elo.A = 1510, elo.B = 1490))
  expect_equal(
    elo.calc(dat$wins.A, dat$dummy.A, dat$dummy.B, k = dat$k.column),
    elo.calc(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
  )
  expect_equal(
    elo.calc(dat$wins.A, dat$dummy.A, dat$dummy.B, k = dat$k.column, adjust.A = 10, adjust.B = 20),
    elo.calc(wins.A ~ adjust(dummy.A, 10) + adjust(dummy.B, 20) + k(k.column), data = dat)
  )
  expect_equal(
    elo.calc(wins.A ~ dummy.A + dummy.B, data = dat, k = 20),
    elo.calc(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
  )
})

test_that("k works with two columns (#45)", {
  expect_equal(
    elo.calc(wins.A ~ dummy.A + dummy.B + k(k.column, 2*k.column), data = dat),
    data.frame(elo.A = c(1510, 1510, 1490), elo.B = c(1480, 1480, 1520))
  )
  expect_equal(
    elo.calc(dat$wins.A, dat$dummy.A, dat$dummy.B, k = cbind(dat$k.column, 2*dat$k.column)),
    elo.calc(wins.A ~ dummy.A + dummy.B + k(k.column, 2*k.column), data = dat)
  )
  expect_equal(
    elo.calc(dat$wins.A, dat$dummy.A, dat$dummy.B, k = cbind(dat$k.column, 2*dat$k.column), adjust.A = 10, adjust.B = 20),
    elo.calc(wins.A ~ adjust(dummy.A, 10) + adjust(dummy.B, 20) + k(k.column, 2*k.column), data = dat)
  )
})



