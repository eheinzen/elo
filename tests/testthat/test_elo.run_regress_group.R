context("Testing the elo.run function for group() and regress()")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("basic regression works, both as logical and numeric", {

  ref1 <- c("Team A" = 1517.770, "Team B" = 1501.949, "Team C" = 1480.281)
  # check basic regressions: logical and numeric
  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season == 1, 1500, 0.2), k = 20, data = dat), 3),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season == 1, 1500, 0.2), k = 20, data = dat), 3),
    ref1
  )
  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 3),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 3),
    ref1
  )
  expect_eq(
    rnd.fin(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), regressed = FALSE),
    rnd.fin(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), regressed = FALSE),
    ref1
  )
})

test_that("final elos work with regression", {
  expect_eq(
    round(as.vector(final.elos(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), regressed = TRUE)), 3),
    round(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat)$elos.regressed[2, ], 3),
    round(as.vector(final.elos(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), regressed = TRUE)), 3),
    round(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat)$elos.regressed[2, ], 3)
  )
  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 2),
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat, subset = week < 2), 2),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 2),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat, subset = week < 2), 2)
  )
})

test_that("regression works with initial elos", {
  expect_eq(
    rnd.fin(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat, initial.elos = init), regressed = FALSE),
    rnd.fin(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat, initial.elos = init), regressed = FALSE),
    c("Team A" = 1590.870, "Team B" = 1501.457, "Team C" = 1387.673)
  )
})

test_that("regress.unused works", {
  expect_eq(
    rnd.fin(elo.run(wins.A ~ team.A + team.B + regress(week, 1500, 0.2, FALSE), k = 20, data = dat), regressed = TRUE),
    rnd.fin(elo.run2(wins.A ~ team.A + team.B + regress(week, 1500, 0.2, FALSE), k = 20, data = dat), regressed = TRUE),
    c("Team A" = 1515.770, "Team B" = 1501.605, "Team C" = 1485.779)
  )
})

test_that("multiple regress 'to' works", {
  expect_eq(
    rnd.fin(elo.run(wins.A ~ team.A + team.B + regress(week, init, 0.2), k = 20, data = dat, initial.elos = init), regressed = TRUE),
    rnd.fin(elo.run2(wins.A ~ team.A + team.B + regress(week, init, 0.2), k = 20, data = dat, initial.elos = init), regressed = TRUE),
    c("Team A" = 1607.587, "Team B" = 1501.195, "Team C" = 1391.218)
  )
})

test_that("group works()", {
  expect_eq(
    as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat))[2:3, ],
    as.matrix(elo.run(wins.A ~ team.A + team.B + group(week), k = 20, data = dat)),
    as.matrix(elo.run2(wins.A ~ team.A + team.B, k = 20, data = dat))[2:3, ],
    as.matrix(elo.run2(wins.A ~ team.A + team.B + group(week), k = 20, data = dat))
  )
  expect_eq(
    as.matrix(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat))[2:3, ],
    as.matrix(elo.run(wins.A ~ team.A + team.B + group(c(FALSE, TRUE, TRUE)), k = 20, data = dat)),
    as.matrix(elo.run2(wins.A ~ team.A + team.B, k = 20, data = dat))[2:3, ],
    as.matrix(elo.run2(wins.A ~ team.A + team.B + group(c(FALSE, TRUE, TRUE)), k = 20, data = dat))
  )
})

test_that("'group()' and 'regress()' both work", {
  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 2:3),
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2) + group(week), k = 20, data = dat)),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 2:3),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2) + group(week), k = 20, data = dat))
  )
})

test_that("'formula' can be in any order", {
  expect_eq(
    rnd.mat(elo.run(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 2:3),
    rnd.mat(elo.run(wins.A ~ k(k.column) + team.A + regress(season, 1500, 0.2) + group(week) + team.B, data = dat)),
    rnd.mat(elo.run2(wins.A ~ team.A + team.B + regress(season, 1500, 0.2), k = 20, data = dat), 2:3),
    rnd.mat(elo.run2(wins.A ~ k(k.column) + team.A + regress(season, 1500, 0.2) + group(week) + team.B, data = dat))
  )
})

results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B + regress(season, 1500, 0.2), data = dat, k = 20)
results2 <- elo.run2(wins.A ~ adjust(team.A, 10) + team.B + regress(season, 1500, 0.2), data = dat, k = 20)
test_that("prediction works correctly", {
  newdat <- data.frame(team.A = "Team A", team.B = "Team B")
  expect_identical(
    predict(results, newdata = newdat),
    elo.prob(final.elos(results)["Team A"], final.elos(results)["Team B"], adjust.A = 10),
    predict(results2, newdata = newdat),
    elo.prob(final.elos(results2)["Team A"], final.elos(results2)["Team B"], adjust.A = 10)
  )
  expect_equal(length(predict(results)), nrow(dat))
  expect_equal(length(predict(results2)), nrow(dat))
})

results2 <- elo.run(wins.A ~ adjust(team.A, 10) + team.B + regress(season, 1500, 0.2), data = dat,
                    k = 20, initial.elos = c("Team A" = 1600, "Team B" = 1500, "Team C" = 1400))
test_that("#25: Deep copying with regressed", {
  expect_equal(results$initial.elos, c("Team A" = 1500, "Team B" = 1500, "Team C" = 1500))
  expect_equal(unname(as.matrix(results)[1, "Team C"]), 1500)

  expect_equal(results2$initial.elos, c("Team A" = 1600, "Team B" = 1500, "Team C" = 1400))
  expect_equal(unname(as.matrix(results2)[1, "Team C"]), 1400)
})

