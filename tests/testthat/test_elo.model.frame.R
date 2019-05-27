context("Testing the elo.model.frame function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("Basic model.frame stuff works", {
  expect_identical(
    dim(elo.model.frame(wins.A ~ team.A + team.B, data = dat, k = 20)),
    c(3L, 4L)
  )

  expect_identical(
    dim(elo.model.frame(wins.A ~ team.A + team.B, data = dat, k = 20,
                        required.vars = c("wins", "elos", "k"))),
    c(3L, 6L)
  )

  expect_identical(
    dim(elo.model.frame(wins.A ~ team.A + team.B, data = dat, k = 20,
                        required.vars = c("wins", "elos", "k", "group", "regress", "neutral", "weights"))),
    c(3L, 10L)
  )

  expect_identical(
    elo.model.frame(wins.A ~ team.A + team.B, data = dat, required.vars = c("neutral", "weights"))$home.field,
    rep(1, nrow(dat))
  )

  expect_identical(
    elo.model.frame(wins.A ~ team.A + team.B, data = dat, required.vars = c("neutral", "weights"))$weights,
    rep(1, nrow(dat))
  )

  expect_identical(
    dim(elo.model.frame(~ team.A + team.B, data = dat)),
    c(3L, 4L)
  )
})

dat2 <- dat
dat2$k.column <- "a"
dat2$wins.A[2] <- 2
dat2$neutral <- rep(2, nrow(dat2))

test_that("Certain errors are issued appropriately", {
  expect_error(elo.model.frame(wins.A ~ team.A, data = dat), "specified correctly")
  expect_error(elo.model.frame(~ team.A + team.B + k(k.column), data = dat, required.vars = c("wins", "elos")),
               "A 'wins' component")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B, data = dat, required.vars = c("k", "elos")),
               "'k' is not in")
  expect_warning(elo.model.frame(wins.A ~ team.A + team.B + k(k.column), data = dat, k = 20),
                 "argument being ignored")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B, data = dat2, k = 20, required.vars = c("wins", "elos")),
               "between 0 and 1")
  expect_error(elo.model.frame(~ team.A + team.B + k(k.column), data = dat2, required.vars = c("k", "elos")),
               "numeric and non-NA")
  expect_warning(elo.model.frame(~ team.A + team.B + neutral(neutral), data = dat2, required.vars = "neutral"),
                 "values aren't 0 or 1")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B + k(k.column), data = dat2), NA)
})

test_that("is.na(adjust()) works (#41)", {
  expect_true(all(is.na(adjust(1:5, NA_real_))))
  expect_true(sum(is.na(adjust(1:3, c(1, NA, 3)))) == 1)
  expect_error(elo.model.frame(wins.A ~ adjust(team.A, NA_real_) + team.B, data = dat, k = 20), "non-missing")
})

test_that("elo.model.frame() obeys na.action w.r.t. adjustments (#40)", {
  expect_equal(dim(elo.model.frame(replace(wins.A, 1, NA) ~ adjust(team.A, 10) + team.B,
                                   data = dat, k = 20, required.vars = c("elos", "k"))), c(2, 5))
})
