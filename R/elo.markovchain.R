#' \code{elo.markovchain}
#'
#' Compute a Markov chain model for a matchup.
#'
#' @inheritParams elo.glm
#' @param weights A vector of weights. Note that these weights are used in the Markov Chain model,
#'   but not the regression.
#' @param k The probability that the winning team is better given that they won. See details.
#' @examples
#' elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   subset = points.Home != points.Visitor, k = 0.7)
#'
#' elo.markovchain(mov(points.Home, points.Visitor) ~ team.Home + team.Visitor, family = "gaussian",
#'   data = tournament, k = 0.7)
#' @details
#'   See the vignette for details on this method. The probabilities we call 'k' purely for convenience.
#'   The differences in assigned scores (from the stationary distribution pi) are fed into a logistic
#'   regression model to predict wins or (usually) a linear model to predict margin of victory.
#'   It is also possible to adjust the regression by setting the second argument of
#'    \code{\link{adjust}()}. As in \code{\link{elo.glm}},
#'   the intercept represents the home-field advantage. Neutral fields can be indicated
#'   using the \code{\link{neutral}()} function, which sets the intercept to 0.
#'
#'   Note that by assigning probabilities in the right way, this function emits the
#'   Logistic Regression Markov Chain model (LRMC).
#' @references Kvam, P. and Sokol, J.S. A logistic regression/Markov chain model for NCAA basketball.
#'   Naval Research Logistics. 2006. 53; 788-803.
#' @seealso \code{\link[stats]{glm}}, \code{\link{summary.elo.markovchain}}, \code{\link{score}},
#'   \code{\link{mov}}, \code{\link{elo.model.frame}}
#' @name elo.markovchain
NULL
#> NULL

#' @rdname elo.markovchain
#' @export
elo.markovchain <- function(formula, data, family = "binomial", weights, na.action, subset, k = NULL, ..., running = FALSE, skip = 0)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "neutral", "weights", "k")
  Call$ncol.k <- 2
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- check_elo_markovchain_vars(mf)
  all.teams <- attr(dat, "teams")
  grp <- mf$group

  # we use the convention Ax = x
  out <- do.call(eloMarkovChain, dat)
  if(any(abs(colSums(out[[1]]) - 1) > sqrt(.Machine$double.eps))) warning("colSums(transition matrix) may not be 1")

  eig <- eigen(out[[1]])
  vec <- as.numeric(eig$vectors[, 1])
  vec <- stats::setNames(vec / sum(vec), all.teams)
  difference <- mean_vec_subset_matrix(vec, dat$teamA+1) - mean_vec_subset_matrix(vec, dat$teamB+1)
  mc.dat <- data.frame(wins.A = mf$wins.A, home.field = mf$home.field, difference = difference)
  if(!all(mf$adj.A == 0)) mc.dat$adj.A <- mf$adj.A
  if(!all(mf$adj.B == 0)) mc.dat$adj.B <- mf$adj.B
  mc.glm <- stats::glm(wins.A ~ . - 1, family = family, data = mc.dat)
  out <- list(
    fit = mc.glm,
    weights = mf$weights,
    transition = out[[1]],
    n.games = out[[2]],
    pi = vec,
    eigenvalue = as.numeric(eig$values[1]),
    y = mc.glm$y,
    fitted.values = mc.glm$fitted.values,
    teams = all.teams,
    group = grp,
    elo.terms = Terms,
    na.action = stats::na.action(mf),
    outcome = attr(mf, "outcome")
  )

  if(running)
  {
    ftd <- rep(0, times = nrow(mc.dat))
    grp2 <- group_to_int(grp, skip)
    y <- dat$winsA
    adj <- cbind(mf$home.field, mf$adj.A, mf$adj.B)

    for(i in setdiff(seq_len(max(grp2)), seq_len(skip)))
    {
      if(i == 1) next
      sbst <- grp2 %in% 1:(i-1)
      dat.tmp <- dat
      dat.tmp$winsA <- dat.tmp$winsA[sbst]
      dat.tmp$k <- dat.tmp$k[sbst, , drop = FALSE]
      dat.tmp$weights <- dat.tmp$weights[sbst]
      dat.tmp$teamA <- dat.tmp$teamA[sbst, , drop = FALSE]
      dat.tmp$teamB <- dat.tmp$teamB[sbst, , drop = FALSE]

      eig <- eigen(do.call(eloMarkovChain, dat.tmp)[[1]])
      vec <- as.numeric(eig$vectors[, 1])
      vec <- stats::setNames(vec / sum(vec), all.teams)
      difference <- mean_vec_subset_matrix(vec, dat$teamA+1) - mean_vec_subset_matrix(vec, dat$teamB+1)

      # tmpfit <- stats::glm(dat$winsA ~ difference, subset = sbst, family = "binomial")
      # ftd[grp2 == i] <- predict(tmpfit, newdata = data.frame(difference = difference[grp2 == i]), type = "link")

      coeff <- stats::glm.fit(cbind(difference, adj)[sbst, , drop=FALSE],
                              dat.tmp$winsA, family = mc.glm$family, control = mc.glm$control)$coefficients
      ftd[grp2 == i] <- apply(cbind(difference, adj)[grp2 == i, , drop=FALSE], 1, function(x) sum(x * coeff, na.rm = TRUE))
    }
    out$running.values <- mc.glm$family$linkinv(ftd)
  }

  structure(out, class = c(if(running) "elo.running", "elo.markovchain"))
}

#' @export
print.elo.markovchain <- function(x, ...)
{
  cat("\nAn object of class 'elo.markovchain', containing information on ", length(x$teams),
      " teams and ", sum(x$n.games)/2, " matches.\n\n", sep = "")
  invisible(x)
}
