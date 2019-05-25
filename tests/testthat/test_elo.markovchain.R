context("Testing the elo.markovchain function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]
trn$neutral <- replace(rep(0, times = nrow(trn)), c(3, 30), 1)

test_that("elo.markovchain is working correctly", {
  tmp.mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn, k = 0.7)

  expect_equal(tmp.mc$eigenvalue, 1)
  expect_equal(sum(tmp.mc$pi), 1)
  expect_equal(colSums(tmp.mc$transition), rep(1, ncol(tmp.mc$transition)))
  expect_equal(tmp.mc$n.games, as.vector(table(c(trn$team.Home, trn$team.Visitor))))

  tmp.mc.adj <- elo.markovchain(score(points.Home, points.Visitor) ~ adjust(team.Home, neutral) + team.Visitor,
                                data = trn, k = 0.7)
  expect_error(predict(tmp.mc.adj, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")), "neutral")
  expect_error(predict(tmp.mc, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")), NA)
})


