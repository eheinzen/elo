context("Testing the players() function")

test_that("elo.run works with players()", {
  expect_identical(
    rnd.mat(elo.run(wins.A ~ players(p1.A, p2.A) + dummy.B, k = 20, data = dat, initial.elos = init.ply), 3),
    c("Player 1" = 749.857, "Player 2" = 759.928, "Player 3" = 749.785)
  )

  expect_identical(
    rnd.mat(elo.run(wins.A ~ players(p1.A, p2.A, weights = c(0.75, 0.25)) + dummy.B, k = 20,
                    data = dat, initial.elos = init.ply), 3),
    c("Player 1" = 754.928, "Player 2" = 759.946, "Player 3" = 744.767)
  )


  tmp <- c("Player 1" = 747.737, "Player 2" = 759.784, "Player 3" = 747.521,
           "Player 4" = 597.479, "Player 5" = 890.216, "Player 6" = 757.263)
  expect_identical(
    rnd.mat(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B), k = 20,
                    data = dat, initial.elos = init.ply), 3),
    tmp
  )
  expect_identical(
    rnd.fin(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B), k = 20,
                    data = dat, initial.elos = init.ply)),
    tmp
  )
})


test_that("elo.run works with adjust(players())", {
  expect_identical(
    rnd.fin(elo.run(wins.A ~ adjust(players(p1.A, p2.A), 20) + players(p1.B, p2.B), k = 20,
                    data = dat, initial.elos = init.ply)),
    c("Player 1" = 747.239, "Player 2" = 759.223, "Player 3" = 747.036,
      "Player 4" = 598.251, "Player 5" = 890.777, "Player 6" = 757.474)
  )
})

test_that("'group()' and 'regress()' work with players()", {
  tmp <- c("Player 1" = 747.117, "Player 2" = 758.827, "Player 3" = 747.944,
           "Player 4" = 628.056, "Player 5" = 861.173, "Player 6" = 756.883)

  # as.matrix gives right regression results
  expect_identical(
    rnd.mat(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B) + regress(season, 750, 0.2),
                    k = 20, data = dat, initial.elos = init.ply), 3),
    tmp
  )

  # final.elos gives right regression results
  expect_identical(
    rnd.fin(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B) + regress(season, 750, 0.2),
                    k = 20, data = dat, initial.elos = init.ply)),
    tmp
  )

  # regression works right the second time
  expect_identical(
    rnd.fin(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B) + regress(season, 750, 0.2),
                    k = 20, data = dat, initial.elos = init.ply), regressed = TRUE),
    c("Player 1" = 747.693, "Player 2" = 757.062, "Player 3" = 748.355,
      "Player 4" = 652.445, "Player 5" = 838.938, "Player 6" = 755.507)
  )

  # regression works right the second time
  expect_identical(
    rnd.fin(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B) +
                      regress(season, init.ply, 0.2),
                    k = 20, data = dat, initial.elos = init.ply), regressed = TRUE),
    c("Player 1" = 747.407, "Player 2" = 757.062, "Player 3" = 748.069,
      "Player 4" = 598.731, "Player 5" = 892.938, "Player 6" = 755.793)
  )

  # as.matrix works right for grouping
  expect_identical(
    rnd.mat(elo.run(wins.A ~ players(p1.A, p2.A) + players(p1.B, p2.B) +
                      regress(week, 750, 0.2) + group(week),
                    k = 20, data = dat, initial.elos = init.ply)),
    matrix(c(755, 747.137, 760, 758, 755, 747.137, 590, 628.863, 890, 862, 750, 756.863), nrow = 2,
           dimnames = list(NULL, names(init.ply)))
  )
})

test_that("elo.prob works with players()", {
  expect_identical(
    elo.prob(c(750+700, 700+650, 650+750), c(600+900, 600+900, 600+750)),
    elo.prob( ~ players(p1.A, p2.A) + players(p1.B, p2.B), data = dat, elos = init.ply2)
  )
  expect_identical(
    elo.prob(c(750+700+10, 700+650+10, 650+750+10), c(600+900, 600+900, 600+750)),
    elo.prob( ~ adjust(players(p1.A, p2.A), 10) + players(p1.B, p2.B), data = dat, elos = init.ply2)
  )
})


results <- elo.run(wins.A ~ adjust(players(p1.A, p2.A), 10) + players(p1.B, p2.B), data = dat, k = 20)
test_that("prediction works correctly with players()", {
  newdat <- data.frame(p1.A = "Player 1", p2.A = "Player 4", p1.B = "Player 5", p2.B = "Player 6")
  expect_identical(
    predict(results, newdata = newdat),
    elo.prob(sum(final.elos(results)[c("Player 1", "Player 4")]),
             sum(final.elos(results)[c("Player 5", "Player 6")]), adjust.A = 10)
  )
  expect_equal(length(predict(results)), nrow(dat))
})

test_that("auc() works correctly with players()", {
  expect_equal(auc(results), 0)
})


test_that("xtfrm error goes away (#61)", {
  test_players_run <- data.frame(
    home_team_win = c(1,0,0,1,0,1),
    playerid_1 = c("1","7","13","14","13","14"),
    playerid_2 = c("2","8","15","16","15","16"),
    playerid_3 = c("3","9","17","18","17","18"),
    playerid_6 = c("4","10","19","20","19","20"),
    playerid_7 = c("5","11","21","22","21","22"),
    playerid_8 = c("6","12","23","24","23","24")
  )

  expect_error(elo.run(home_team_win ~ players(playerid_1, playerid_2, playerid_3) +
                         players(playerid_6, playerid_7, playerid_8),
                       data = test_players_run, k = 20), NA)
})





