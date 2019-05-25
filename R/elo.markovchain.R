#' \code{elo.markovchain}
#'
#' Compute a Markov chain model for a matchup.
#'
#' @inheritParams elo.glm
#' @param weights A vector of weights.
#' @param k The probability that the winning team is better given that they won. See details.
#' @examples
#' elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   subset = points.Home != points.Visitor, k = 0.7)
#' @details
#'   See the vignette for details on this method. The probabilities we call 'k' purely for convenience.
#'   The differences in assigned scores (from the stationary distribution pi) are fed into a logistic
#'   regression model to predict wins. This logistic regession accepts the arguments of \code{\link{adjust}()}
#'   in \code{formula}. See the vignette for more details
#'
#'   Note that by assigning probabilities in the right way, this function emits the
#'   Logistic Regression Markov Chain model (LRMC). It is also possible to adjust the logistic
#'   regression for home/away/neutral status by setting the second argument of \code{\link{adjust}()}.
#' @references Kvam, P. and Sokol, J.S. A logistic regression/Markov chain model for NCAA basketball.
#'   Naval Research Logistics. 2006. 53; 788-803.
#' @name elo.markovchain
NULL
#> NULL

#' @rdname elo.markovchain
#' @export
elo.markovchain <- function(formula, data, weights, na.action, subset, k = NULL, ..., running = FALSE, skip = 0)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "neutral", "weights", "k")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- check_elo_markovchain_vars(mf)
  all.teams <- attr(dat, "teams")
  grp <- mf$group

  # we use the convention Ax = x
  out <- do.call(eloMarkovChain, c(as.list(dat), list(nTeams = length(all.teams))))
  if(any(abs(colSums(out[[1]]) - 1) > sqrt(.Machine$double.eps))) warning("colSums(transition matrix) may not be 1")

  eig <- eigen(out[[1]])
  vec <- as.numeric(eig$vectors[, 1])
  vec <- stats::setNames(vec / sum(vec), all.teams)

  mc.dat <- data.frame(wins.A = dat$winsA, home.field = mf$home.field, difference = vec[dat$teamA+1] - vec[dat$teamB+1])
  if(!all(mf$adj.A == 0)) mc.dat$adj.A <- mf$adj.A
  if(!all(mf$adj.B == 0)) mc.dat$adj.B <- mf$adj.B
  mc.glm <- stats::glm(wins.A ~ . - 1, family = "binomial", data = mc.dat)
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
    na.action = stats::na.action(mf)
  )

  if(running)
  {
    ftd <- rep(0, times = nrow(dat))
    grp2 <- group_to_int(grp, skip)
    y <- dat$winsA
    adj <- cbind(mf$home.field, mf$adj.A, mf$adj.B)

    for(i in setdiff(seq_len(max(grp2)), seq_len(skip)))
    {
      if(i == 1) next
      sbst <- grp2 %in% 1:(i-1)
      dat.tmp <- dat[sbst, ]

      eig <- eigen(do.call(eloMarkovChain, c(as.list(dat.tmp), list(nTeams = length(all.teams))))[[1]])
      vec <- as.numeric(eig$vectors[, 1])
      vec <- stats::setNames(vec / sum(vec), all.teams)
      difference <- vec[dat$teamA+1] - vec[dat$teamB+1]

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
