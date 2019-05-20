context("Testing the summary functions")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

test_that("auc.elo.run works", {
  results <- elo.run(wins.A ~ adjust(team.A, 10) + team.B, data = rbind(dat, dat), k = 20)

  expect_equal(
    as.numeric(pROC::auc(results$elos[, 4], results$elos[, 3])),
    auc(results)
  )
})

test_that("auc.elo.glm works", {
  results <- elo.glm(wins.A ~ team.A + team.B, data = rbind(dat, dat))
  expect_equal(
    as.numeric(pROC::auc(results$y, results$fitted.values)),
    auc(results)
  )

  results <- elo.glm(replace(wins.A, 1, NA) ~ team.A + team.B, data = rbind(dat, dat), na.action = na.exclude)
  expect_false(is.na(auc(results)))
})
