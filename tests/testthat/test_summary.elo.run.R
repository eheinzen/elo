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
