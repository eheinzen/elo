context("Testing the auxiliary  functions")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = rbind(dat, dat), k = 20)
results.na <- elo.run(replace(wins.A, 1, NA) ~ adjust(team.A, 10) + team.B,
                      data = rbind(dat, dat), k = 20, na.action = na.exclude)

results.glm <- elo.glm(wins.A ~ team.A + team.B, data = rbind(dat, dat))
results.glm.na <- elo.glm(replace(wins.A, 1, NA) ~ team.A + team.B, data = rbind(dat, dat), na.action = na.exclude)

test_that("fitted and residuals works with NAs (#39)", {
  expect_true(is.na(fitted(results.glm.na)[1]))
  expect_true(is.na(fitted(results.na)[1]))
  expect_true(is.na(residuals(results.glm.na)[1]))
  expect_true(is.na(residuals(results.na)[1]))
})

test_that("auc.elo.run works", {
  expect_equal(
    as.numeric(pROC::auc(results$elos[, 4], results$elos[, 3])),
    auc(results)
  )
})

test_that("auc.elo.glm works (#37)", {
  expect_equal(
    as.numeric(pROC::auc(results.glm$y, results.glm$fitted.values)),
    auc(results.glm)
  )

  expect_false(is.na(auc(results.glm.na)))
})

test_that("favored.elo.run works", {
  results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = rbind(dat, dat), k = 20)

  expect_equal(
    favored(results),
    as.table(matrix(c(1, 0, 1, 4, 0, 0), nrow = 3,
                    dimnames = list(Favored = c("TRUE", "(tie)", "FALSE"), Actual = c("0", "1"))))
  )
})

test_that("favored.elo.glm works (#38)", {
  results <- elo.glm(wins.A ~ team.A + team.B, data = rbind(dat, dat))

  expect_equal(
    favored(results),
    as.table(matrix(c(0, 0, 2, 4, 0, 0), nrow = 3,
                    dimnames = list(Favored = c("TRUE", "(tie)", "FALSE"), Actual = c("0", "1"))))
  )
})

test_that("rank.teams works", {
  er <- elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, 30) + team.Visitor, data = tournament, k = 20,
                subset = points.Home != points.Visitor)
  em <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament, k = 0.7,
                        subset = points.Home != points.Visitor)
  eg <- elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                subset = points.Home != points.Visitor)
  ew <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                   subset = points.Home != points.Visitor)


  expect_equal(unname(rank.teams(er)), c(1, 7, 3, 8, 5, 2, 4, 6))
  expect_equal(names(rank.teams(er)), er$teams)

  expect_equal(unname(rank.teams(em)), c(1, 7, 3, 8, 4, 2, 6, 5))
  expect_equal(names(rank.teams(em)), em$teams)

  expect_equal(unname(rank.teams(eg)), c(1, 7, 3, 8, 5, 2, 4, 6))
  expect_equal(names(rank.teams(eg)), eg$teams)

  expect_equal(unname(rank.teams(ew)), c(1, 7, 4, 8, 5, 2, 3, 6))
  expect_equal(names(rank.teams(ew)), ew$teams)
})



