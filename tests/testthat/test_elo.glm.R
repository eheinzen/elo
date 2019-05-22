context("Testing the elo.glm function")

###########################################################################################################
#### Do some simple checks
###########################################################################################################

trn <- tournament
trn$diff <- score(trn$points.Home, trn$points.Visitor)
trn <- trn[trn$diff %in% 0:1, ]

test_that("Running elo.glm works", {
  tmp.glm     <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn)
  tmp.glm.sub <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, subset = week %in% 1:6)
  tmp.glm.run <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, running = TRUE, skip = 5)
  expect_equal(fitted(tmp.glm.run)[trn$week == 14], fitted(tmp.glm)[trn$week == 14])
  expect_equal(fitted(tmp.glm.run)[trn$week %in% 1:6], fitted(tmp.glm.sub))

  expect_equal(mse(tmp.glm.run, subset = trn$week == 14), mse(tmp.glm, subset = trn$week == 14))
  expect_equal(mse(tmp.glm.run, subset = trn$week %in% 1:6), mse(tmp.glm.sub))
})

test_that("Errors are thrown appropriately", {
  expect_error(elo.glm(diff ~ team.Home + team.Visitor, data = trn, running = TRUE, skip = -1),
               "0 and 51 (inclusive)", fixed = TRUE)
  expect_error(elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, running = TRUE, skip = 15),
               "0 and 14 (inclusive)", fixed = TRUE)
})

test_that("Running elo.glm works with weights", {
  tmp.glm.sub <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, subset = week %in% 1:6, weights = 1:nrow(trn))
  tmp.glm.run <- elo.glm(diff ~ team.Home + team.Visitor + group(week), data = trn, running = TRUE, skip = 5, weights = 1:nrow(trn))
  expect_equal(fitted(tmp.glm.run)[trn$week %in% 1:6], fitted(tmp.glm.sub))
  expect_equal(mse(tmp.glm.run, subset = trn$week %in% 1:6), mse(tmp.glm.sub))
})
