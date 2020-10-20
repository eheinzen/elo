context("Testing the elo.run.multiteam function")

test_that("Basic Elo calculations work", {
  expect_eq(
    rnd.mat(elo.run.multiteam(~ multiteam("Team A", "Team B", "Team C"), k = 20)),
    rnd.mat(elo.run.multiteam(~ multiteam("Team A", "Team B", "Team C") + group(1), k = 20)),
    rnd.mat(elo.run(wins.A ~ team.A + team.B + group(rep(1, 3)), k = 20, data = dat)),
    c("Team A" = 1520, "Team B" = 1500, "Team C" = 1480)
  )
  expect_eq(
    rnd.mat(elo.run.multiteam(~ multiteam("Team A", "Team B"), data = dat, k = 20)),
    rnd.mat(elo.run(wins.A ~ team.A + team.B, k = 20, data = dat, subset = 1)),
    c("Team A" = 1510, "Team B" = 1490)
  )

  expect_eq(
    rnd.mat(elo.run.multiteam(~ multiteam(c("Team A", "Team A"), c("Team B", "Team B"), c("Team C", NA)), k = 20), 2),
    rnd.mat(elo.run(wins.A ~ team.A + team.B + group(c(rep(1, 3), 2)), k = 20, data = dat[c(1:3, 1), ]), 2),
    c("Team A" = 1529.425, "Team B" = 1490.575, "Team C" = 1480)
  )
})

test_that("prediction works correctly", {
  erm <- elo.run.multiteam(~ multiteam(Place_1, Place_2, Place_3, Place_4), data = tournament.multiteam, subset = -(27:28), k = 20)
  newdat <- tournament.multiteam[27:28, ]
  expect_equal(
    rowSums(predict(erm, newdata = newdat)),
    c(1, 1)
  )
  expect_equal(
    predict(erm, newdata = newdat),
    matrix(c(
      10^(final.elos(erm)[1:4]/400) / sum(10^(final.elos(erm)[1:4]/400)),
      10^(final.elos(erm)[c(7:6, 8, 5)]/400) / sum(10^(final.elos(erm)[5:8]/400))
    ), nrow = 2, byrow = TRUE, dimnames = NULL)
  )
})
