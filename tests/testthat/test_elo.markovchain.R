context("Testing the elo.markovchain function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]
trn$neut <- replace(rep(0, times = nrow(trn)), c(3, 30), 1)

test_that("elo.markovchain is working correctly", {
  tmp.mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn, k = 0.7)

  expect_equal(tmp.mc$eigenvalue, 1)
  expect_equal(sum(tmp.mc$pi), 1)
  expect_equal(colSums(tmp.mc$transition), rep(1, ncol(tmp.mc$transition)))
  expect_equal(tmp.mc$n.games, as.vector(table(c(trn$team.Home, trn$team.Visitor))))
})

test_that("predict.elo.markovchain is working correctly", {
  tmp.mc.adj <- elo.markovchain(score(points.Home, points.Visitor) ~ adjust(team.Home, neut) + team.Visitor,
                                data = trn, k = 0.7)
  expect_error(predict(tmp.mc.adj, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")),
               "'neut' not found")

  tmp.mc.neu <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neut),
                                data = trn, k = 0.7)
  expect_error(predict(tmp.mc.neu, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")),
               "'neut' not found")

  tmp.mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = trn, k = 0.7)
  expect_error(predict(tmp.mc, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")), NA)
})

test_that("elo.markovchain(running=TRUE) works", {
  tmp.mc.run <- elo.markovchain(diff ~ team.Home + team.Visitor + group(week),
                                data = trn, k = 0.7, running = TRUE, skip = 5)

  expect_equal(fitted(tmp.mc.run)[1:19], rep(0.5, 19))

  tmp.mc <- elo.markovchain(diff ~ team.Home + team.Visitor, data = trn, k = 0.7)
  expect_equal(predict(tmp.mc, newdata=head(trn, 2)), predict(tmp.mc.run, newdata=head(trn, 2)))


})


test_that("adjust() works in elo.markovchain()", {
  tmp.mc.adj0 <- elo.markovchain(diff ~ team.Home + adjust(team.Visitor, 0) + group(week), data = trn,
                                 running = TRUE, skip = 5, k = 0.7)
  tmp.mc.adj1 <- elo.markovchain(diff ~ team.Home + adjust(team.Visitor, c(rep(0, 50), 1)) + group(week),
                                 data = trn, running = TRUE, skip = 5, k = 0.7)
  tmp.mc.noad <- elo.markovchain(diff ~ team.Home + team.Visitor + group(week), data = trn,
                                 running = TRUE, skip = 5, k = 0.7)
  expect_equal(fitted(tmp.mc.adj0), fitted(tmp.mc.adj1))
  expect_equal(fitted(tmp.mc.adj0), fitted(tmp.mc.noad))
})
