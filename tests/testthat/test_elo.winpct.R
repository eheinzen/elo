context("Testing the elo.winpct function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]
trn$neut <- replace(rep(0, times = nrow(trn)), c(3, 30), 1)

test_that("elo.winpct is working correctly", {
  tmp.wl <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn)

  expect_equal(unname(tmp.wl$win.pct), c(10/13, 4/14, 7/12, 2/13, 7/13, 9/14, 6/10, 6/13))
  expect_equal(sum(tmp.wl$win.pct * tmp.wl$n.games), 51)
  expect_equal(tmp.wl$n.games, as.vector(table(c(trn$team.Home, trn$team.Visitor))))

  tmp.wl.wt <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn, weights = 1:51)
  expect_equal(sum(tmp.wl.wt$win.pct * tmp.wl.wt$n.games), sum(1:51))
  expect_equal(tmp.wl.wt$n.games, as.vector(xtabs(c(1:51, 1:51) ~ c(trn$team.Home, trn$team.Visitor))))

  tmp.wl.players <- elo.winpct(score(points.Home, points.Visitor) ~ players(team.Home, "Zealous Zebras") + players(team.Visitor, "Yelling Yaks"), data = trn)
  expect_equal(sum(tmp.wl.players$win.pct * tmp.wl.players$n.games), 51)
  expect_equal(tmp.wl.players$n.games, 0.5*as.vector(table(c(trn$team.Home, trn$team.Visitor, rep(c("Zealous Zebras", "Yelling Yaks"), each = 51)))))
})
