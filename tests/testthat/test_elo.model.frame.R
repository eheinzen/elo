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
                        required.vars = c("wins", "teams", "k"))),
    c(3L, 6L)
  )

  expect_identical(
    dim(elo.model.frame(wins.A ~ team.A + team.B, data = dat, k = 20,
                        required.vars = c("wins", "teams", "k", "group", "regress"))),
    c(3L, 8L)
  )

  expect_identical(
    dim(elo.model.frame(~ team.A + team.B, data = dat)),
    c(3L, 4L)
  )
})

dat2 <- dat
dat2$k.column <- "a"
dat2$wins.A[2] <- 2

test_that("Certain errors are issued appropriately", {
  expect_error(elo.model.frame(wins.A ~ team.A, data = dat), "specified correctly")
  expect_error(elo.model.frame(~ team.A + team.B + k(k.column), data = dat, required.vars = c("wins", "teams")),
               "A 'wins' component")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B, data = dat, required.vars = c("k", "teams")),
               "'k' is not in")
  expect_warning(elo.model.frame(wins.A ~ team.A + team.B + k(k.column), data = dat, k = 20),
                 "argument being ignored")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B, data = dat2, k = 20, required.vars = c("wins", "teams")),
               "between 0 and 1")
  expect_error(elo.model.frame(~ team.A + team.B + k(k.column), data = dat2, required.vars = c("k", "teams")),
               "numeric and non-NA")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B + k(k.column), data = dat2), NA)
})

