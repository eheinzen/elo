context("Testing the elo.winloss function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]
trn$neut <- replace(rep(0, times = nrow(trn)), c(3, 30), 1)

test_that("elo.winloss is working correctly", {
  tmp.wl <- elo.winloss(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn)

  expect_equal(tmp.wl$win.pct, c(10/13, 4/14, 7/12, 2/13, 7/13, 9/14, 6/10, 6/13))
  expect_equal(tmp.wl$n.games, as.vector(table(c(trn$team.Home, trn$team.Visitor))))
})
