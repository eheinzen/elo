

context("Testing the elo.model.frame function")

df <- data.frame(
  team.A = c("Team A", "Team A", "Team C"),
  team.B = c("Team B", "Team C", "Team B"),
  wins.A = c(1, 1, 0),
  dummy.A = 1500,
  dummy.B = 1500,
  k.column = 20,
  home.field = 10
)

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("Basic model.frame stuff works", {
  expect_identical(
    dim(elo.model.frame(wins.A ~ team.A + team.B, data = df, k = 20)),
    c(3L, 6L)
  )

  expect_identical(
    dim(elo.model.frame(~ team.A + team.B, data = df)),
    c(3L, 4L)
  )
})

df2 <- df
df2$k.column <- "a"
df2$wins.A[2] <- 2

test_that("Certain errors are issued appropriately", {
  expect_error(elo.model.frame(wins.A ~ team.A, data = df), "specified correctly")
  expect_error(elo.model.frame(~ team.A + team.B + k(k.column), data = df, required.vars = c("wins", "teams")),
               "A 'wins' component")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B, data = df, required.vars = c("k", "teams")),
               "'k' is not in")
  expect_warning(elo.model.frame(wins.A ~ team.A + team.B + k(k.column), data = df, k = 20),
                 "argument being ignored")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B, data = df2, k = 20, required.vars = c("wins", "teams")),
               "between 0 and 1")
  expect_error(elo.model.frame(~ team.A + team.B + k(k.column), data = df2, required.vars = c("k", "teams")),
               "numeric and non-NA")
  expect_error(elo.model.frame(wins.A ~ team.A + team.B + k(k.column), data = df2), NA)
})

