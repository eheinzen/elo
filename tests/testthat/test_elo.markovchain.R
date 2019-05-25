context("Testing the elo.markovchain function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]

test_that("elo.markovchain is working correctly", {
  tmp.mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn, k = 0.7)

  expect_equal(tmp.mc$eigenvalue, 1)
  expect_equal(sum(tmp.mc$pi), 1)
  expect_equal(colSums(tmp.mc$transition), rep(1, ncol(tmp.mc$transition)))
  expect_equal(tmp.mc$n.games, as.vector(table(c(trn$team.Home, trn$team.Visitor))))
})
