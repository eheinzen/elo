context("Selecting a constant K on evaluation matches")

tuning.data <- data.frame(
  team.A = rep("A", 8), team.B = rep("B", 8),
  wins.A = c(1, 1, 0.5, 1, 0, 0.5, 0, 1),
  round = rep(1:4, each = 2)
)
tuning.formula <- wins.A ~ team.A + team.B
tuning.rows <- seq_len(nrow(tuning.data)) > 4

test_that("selection agrees with independent elo.run and mse calls", {
  candidates <- c(40, 0, 10, 20)
  result <- elo.tune(tuning.formula, tuning.data, candidates, tuning.rows)
  expected <- vapply(sort(candidates), function(value) {
    mse(elo.run(tuning.formula, tuning.data, k = value), subset = tuning.rows)
  }, numeric(1))
  expect_equal(result$scores, data.frame(k = sort(candidates), n = 4, mse = expected))
  expect_equal(result$best.k, sort(candidates)[which.min(expected)])
  reference <- elo.run(tuning.formula, tuning.data, k = result$best.k)
  expect_equal(result$fit$elos, reference$elos)
  expect_equal(final.elos(result$fit), final.elos(reference))
})

test_that("unscored warm-up matches establish evaluation ratings", {
  result <- elo.tune(tuning.formula, tuning.data, 40, tuning.rows)
  truncated <- elo.run(tuning.formula, tuning.data, k = 40, subset = tuning.rows)
  expect_false(isTRUE(all.equal(result$scores$mse, mse(truncated))))
  expect_false(isTRUE(all.equal(fitted(result$fit)[5], 0.5)))
  expect_equal(result$fit$n.matches, nrow(tuning.data))
})

test_that("draws affect both the loss and later ratings", {
  result <- elo.tune(tuning.formula, tuning.data, 40, seq_len(8) == 3)
  p <- fitted(result$fit)
  expect_equal(result$scores$mse, (0.5 - p[3])^2)
  expect_true(p[3] > p[4])
  expect_true(p[4] > 0.5)
  changed <- tuning.data
  changed$wins.A[3] <- 1
  won <- elo.tune(tuning.formula, changed, 40, seq_len(8) == 3)
  expect_false(isTRUE(all.equal(result$scores$mse, won$scores$mse)))
  expect_true(fitted(won$fit)[4] > p[4])
})

test_that("candidate order cannot change results or supplied initial ratings", {
  initial <- c(A = 1550, B = 1450)
  original <- initial
  first <- elo.tune(tuning.formula, tuning.data, c(0, 10, 40), tuning.rows, initial.elos = initial)
  second <- elo.tune(tuning.formula, tuning.data, c(40, 10, 0), tuning.rows, initial.elos = initial)
  expect_equal(first, second)
  expect_identical(initial, original)
  for(i in seq_len(nrow(first$scores)))
  {
    reference <- elo.run(tuning.formula, tuning.data, k = first$scores$k[i], initial.elos = original)
    expect_equal(first$scores$mse[i], mse(reference, subset = tuning.rows))
  }
})

test_that("later outcomes cannot affect earlier evaluation scores", {
  evaluate <- seq_len(8) %in% 3:5
  before <- elo.tune(tuning.formula, tuning.data, c(10, 40), evaluate)
  changed <- tuning.data
  changed$wins.A[6:8] <- 1 - changed$wins.A[6:8]
  after <- elo.tune(tuning.formula, changed, c(10, 40), evaluate)
  expect_equal(before$scores, after$scores)
  expect_equal(before$best.k, after$best.k)
  expect_equal(before$fit$n.matches, 8)
  expect_false(isTRUE(all.equal(final.elos(before$fit), final.elos(after$fit))))
})

test_that("grouping, regression, and adjustments retain elo.run semantics", {
  form <- wins.A ~ adjust(team.A, 30) + team.B + group(round) + regress(round, 1500, 0.2)
  evaluate <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
  result <- elo.tune(form, tuning.data, c(10, 40), evaluate)
  for(i in seq_len(nrow(result$scores)))
  {
    reference <- elo.run(form, tuning.data, k = result$scores$k[i])
    expect_equal(result$scores$mse[i], mse(reference, subset = evaluate))
  }
  reference <- elo.run(form, tuning.data, k = result$best.k)
  expect_s3_class(result$fit, "elo.run.regressed")
  expect_equal(result$fit$elos, reference$elos)
  expect_equal(result$fit$elos.regressed, reference$elos.regressed)
})

test_that("custom functions are forwarded to the R backend", {
  probability <- function(elo.A, elo.B, adjust.A, adjust.B) {
    pnorm((elo.A + adjust.A - elo.B - adjust.B)/400)
  }
  result <- elo.tune(tuning.formula, tuning.data, c(10, 40), tuning.rows,
                     prob.fun = probability, update.fun = elo.update, verbose = FALSE)
  for(i in seq_len(nrow(result$scores)))
  {
    reference <- elo.run(tuning.formula, tuning.data, k = result$scores$k[i],
                         prob.fun = probability, update.fun = elo.update, verbose = FALSE)
    expect_equal(result$scores$mse[i], mse(reference, subset = tuning.rows))
  }
})

test_that("zero K and exact loss ties have deterministic behavior", {
  result <- elo.tune(tuning.formula, tuning.data, c(40, 0, 10), seq_len(8) == 1)
  expect_equal(result$scores$mse, rep(0.25, 3))
  expect_equal(result$best.k, 0)
  expect_equal(unname(final.elos(result$fit)), c(1500, 1500))
  positive <- elo.tune(tuning.formula, tuning.data, c(40, 10), seq_len(8) == 1)
  expect_equal(positive$best.k, 10)
})

test_that("missing model values never silently remove scored or warm-up rows", {
  for(row in c(1, 6))
  {
    incomplete <- tuning.data
    incomplete$wins.A[row] <- NA_real_
    expect_error(elo.tune(tuning.formula, incomplete, 20, tuning.rows), "missing values")
  }
  incomplete <- tuning.data
  incomplete$team.A[1] <- NA_character_
  expect_error(elo.tune(tuning.formula, incomplete, 20, tuning.rows), "missing values")
  unused <- tuning.data
  unused$irrelevant <- NA_real_
  expect_equal(elo.tune(tuning.formula, unused, 20, tuning.rows)$scores,
               elo.tune(tuning.formula, tuning.data, 20, tuning.rows)$scores)
  old <- options(na.action = "na.omit")
  on.exit(options(old))
  expect_error(elo.tune(tuning.formula, incomplete, 20, tuning.rows), "missing values")
})

test_that("invalid candidate grids and evaluation masks fail clearly", {
  for(values in list(numeric(), NULL, "20", TRUE, -1, NA_real_, NaN, Inf,
                     c(20, 20), c(10, NA_real_), matrix(20), 1+1i))
  {
    expect_error(elo.tune(tuning.formula, tuning.data, values, tuning.rows), "'k'")
  }
  for(rows in list(NULL, logical(), TRUE, rep(FALSE, 8), 1:8, rep(NA, 8),
                   c(TRUE, rep(FALSE, 6), NA), matrix(TRUE, nrow = 8), rep(TRUE, 9)))
  {
    expect_error(elo.tune(tuning.formula, tuning.data, 20, rows), "'evaluate'")
  }
  expect_error(elo.tune(tuning.formula, tuning.data[FALSE, ], 20, logical()), "'data'")
  expect_error(elo.tune(tuning.formula, as.list(tuning.data), 20, tuning.rows), "'data'")
})

test_that("conflicting and misspelled fitting arguments are rejected", {
  expect_error(elo.tune(wins.A ~ team.A + team.B + k(20), tuning.data, 10, tuning.rows), "k\\(\\) term")
  expect_error(elo.tune(tuning.formula, tuning.data, 20, tuning.rows, subset = tuning.rows), "'subset'")
  expect_error(elo.tune(tuning.formula, tuning.data, 20, tuning.rows, na.action = na.omit), "na.fail")
  expect_error(elo.tune(tuning.formula, tuning.data, 20, tuning.rows, na.action = na.fail), "na.fail")
  expect_error(elo.tune(tuning.formula, tuning.data, 20, tuning.rows, 1500), "named arguments")
  expect_error(elo.tune(tuning.formula, tuning.data, 20, tuning.rows, initial.elo = 1500), "named arguments")
  expect_error(elo.tune(tuning.formula, tuning.data, 20, tuning.rows,
                        initial.elos = 1500, initial.elos = 1600), "named arguments")
})

test_that("non-finite evaluation losses are rejected", {
  invalid.probability <- function(elo.A, elo.B, adjust.A, adjust.B) NaN
  expect_error(elo.tune(tuning.formula, tuning.data, 0, tuning.rows,
                        prob.fun = invalid.probability, verbose = FALSE), "Non-finite evaluation MSE")
})
