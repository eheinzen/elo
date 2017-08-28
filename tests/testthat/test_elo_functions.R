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
})

test_that("elo.update works", {
  expect_equal(elo.update(1, 1500, 1500, k = 20), 10)
  expect_equal(
    elo.update(dat$wins.A, dat$dummy.A, dat$dummy.B, k = dat$k.column),
    elo.update(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
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
    elo.calc(wins.A ~ dummy.A + dummy.B, data = dat, k = 20),
    elo.calc(wins.A ~ dummy.A + dummy.B + k(k.column), data = dat)
  )
})
