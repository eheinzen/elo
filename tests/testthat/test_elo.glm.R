context("Testing the elo.glm function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]
trn$neut <- replace(rep(0, times = nrow(trn)), c(3, 30), 1)

test_that("elo.glm(running=TRUE) works", {
  tmp.glm.run <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, running = TRUE, skip = 5)
  expect_equal(
    tmp.glm.run$running.values[44:47],
    unname(predict(glm(wins.A ~ . - 1, data = head(tmp.glm.run$data, -8), family = "binomial"),
                   newdata = tmp.glm.run$data[44:47, ], type = "response"))
  )
  expect_equal(
    tail(tmp.glm.run$running.values, 4),
    unname(predict(glm(wins.A ~ . - 1, data = head(tmp.glm.run$data, -4), family = "binomial"),
                   newdata = tail(tmp.glm.run$data, 4), type = "response"))
  )
  expect_equal(fitted(tmp.glm.run)[1:19], rep(0.5, 19))

  tmp.glm <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn)
  expect_equal(predict(tmp.glm, newdata=head(trn, 2)), predict(tmp.glm.run, newdata=head(trn, 2)))
})

test_that("Errors are thrown appropriately", {
  expect_error(elo.glm(diff ~ team.Home + team.Visitor, data = trn, running = TRUE, skip = -1),
               "0 and 51 (inclusive)", fixed = TRUE)
  expect_error(elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, running = TRUE, skip = 15),
               "0 and 14 (inclusive)", fixed = TRUE)
})

test_that("Running elo.glm works with weights", {
  trn2 <- trn
  trn2$wts <- 1:nrow(trn2)
  tmp.glm.run <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn2, running = TRUE,
                         skip = 5, weights = wts)
  expect_equal(
    tail(tmp.glm.run$running.values, 4),
    unname(predict(glm(wins.A ~ . - 1, data = head(tmp.glm.run$data, -4), family = "binomial", weights = head(trn2$wts, -4)),
                   newdata = tail(tmp.glm.run$data, 4), type = "response"))
  )
})

test_that("predict.elo.glm works correctly", {
  tmp.glm <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn)
  expect_equal(fitted(tmp.glm), predict(tmp.glm))
  expect_error(predict(tmp.glm, newdata = data.frame(team.Home = "Unknown", team.Visitor = "Unknown 2")), "Unknown teams: Unknown, Unknown 2")
  expect_error(predict(tmp.glm, newdata = data.frame(team.Home = "Unknown")), "object 'team.Visitor' not found")

  expect_equal(
    tmp.glm$family$linkinv(sum(coef(tmp.glm) * c(1, 1, -1, rep(0, 5)))),
    unname(predict(tmp.glm, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons")))
  )
  expect_equal(
    tmp.glm$family$linkinv(sum(coef(tmp.glm) * c(1, 1, rep(0, 6)))),
    unname(predict(tmp.glm, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Helpless Hyenas")))
  )

  expect_equal(
    sum(coef(tmp.glm) * c(1, 1, -1, rep(0, 5))),
    unname(predict(tmp.glm, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons"), type = "link"))
  )
  expect_equal(
    sum(coef(tmp.glm) * c(1, 1, rep(0, 6))),
    unname(predict(tmp.glm, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Helpless Hyenas"), type = "link"))
  )

  tmp.glm2 <- elo.glm(diff ~ team.Home + team.Visitor + neutral(neut), data = trn)
  expect_equal(
    tmp.glm2$family$linkinv(sum(coef(tmp.glm2) * c(1, 1, -1, rep(0, 5)))),
    unname(predict(tmp.glm2, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", neut = 0)))
  )
  expect_equal(
    tmp.glm2$family$linkinv(sum(coef(tmp.glm2) * c(0, 1, rep(0, 6)))),
    unname(predict(tmp.glm2, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Helpless Hyenas", neut = 1)))
  )

  tmp.glm.adj <- elo.glm(diff ~ team.Home + adjust(team.Visitor, neut) + group(week), data = trn)
  expect_error(predict(tmp.glm.adj, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")),
               "'neut' not found")

  tmp.glm.neu <- elo.glm(diff ~ team.Home + team.Visitor + neutral(neut) + group(week), data = trn)
  expect_error(predict(tmp.glm.neu, data.frame(team.Home = "Blundering Baboons", team.Visitor = "Athletic Armadillos")),
               "'neut' not found")
})

test_that("adjust() works in elo.glm()", {
  tmp.glm.adj0 <- elo.glm(diff ~ team.Home + adjust(team.Visitor, 0) + group(week), data = trn, running = TRUE, skip = 5)
  tmp.glm.adj1 <- elo.glm(diff ~ team.Home + adjust(team.Visitor, c(rep(0, 50), 1)) + group(week), data = trn, running = TRUE, skip = 5)
  tmp.glm.noad <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, running = TRUE, skip = 5)
  expect_equal(fitted(tmp.glm.adj0), fitted(tmp.glm.adj1))
  expect_equal(fitted(tmp.glm.adj0), fitted(tmp.glm.noad))
})
