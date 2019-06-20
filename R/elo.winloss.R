#' \code{elo.markovchain}
#'
#' Compute a Markov chain model for a matchup.
#'
#' @inheritParams elo.glm
#' @param weights A vector of weights. Note that these are used in calculating wins and losses but
#'   not in the logistic regression.
#' @examples
#' elo.winloss(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   subset = points.Home != points.Visitor)
#' @name elo.winloss
NULL
#> NULL

#' @rdname elo.winloss
#' @export
elo.winloss <- function(formula, data, weights, na.action, subset, ..., running = FALSE, skip = 0)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "neutral", "weights")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- check_elo_winloss_vars(mf)
  all.teams <- attr(dat, "teams")
  grp <- mf$group

  out <- do.call(eloWinLoss, dat)
  vec <- stats::setNames(out[[1]], all.teams)

  dif <- apply(dat$teamA + 1, 1, function(x) mean(vec[x])) - apply(dat$teamB + 1, 1, function(x) mean(vec[x]))
  wl.dat <- data.frame(wins.A = dat$winsA, home.field = mf$home.field, difference = dif)
  if(!all(mf$adj.A == 0)) wl.dat$adj.A <- mf$adj.A
  if(!all(mf$adj.B == 0)) wl.dat$adj.B <- mf$adj.B
  wl.glm <- stats::glm(wins.A ~ . - 1, family = "binomial", data = wl.dat)
  out <- list(
    fit = wl.glm,
    weights = mf$weights,
    win.pct = out[[1]],
    n.games = out[[2]],
    y = wl.glm$y,
    fitted.values = wl.glm$fitted.values,
    teams = all.teams,
    group = grp,
    elo.terms = Terms,
    na.action = stats::na.action(mf)
  )

  if(running)
  {
    ftd <- rep(0, times = nrow(wl.dat))
    grp2 <- group_to_int(grp, skip)
    y <- dat$winsA
    adj <- cbind(mf$home.field, mf$adj.A, mf$adj.B)

    for(i in setdiff(seq_len(max(grp2)), seq_len(skip)))
    {
      if(i == 1) next
      sbst <- grp2 %in% 1:(i-1)
      dat.tmp <- dat
      dat.tmp[c(1:3, 6)] <- lapply(dat.tmp[c(1:3, 6)], `[`, sbst)

      wl <- do.call(eloWinLoss, dat)
      vec <- stats::setNames(wl[[1]], all.teams)

      difference <- vec[dat$teamA+1] - vec[dat$teamB+1]

      # tmpfit <- stats::glm(dat$winsA ~ difference, subset = sbst, family = "binomial")
      # ftd[grp2 == i] <- predict(tmpfit, newdata = data.frame(difference = difference[grp2 == i]), type = "link")

      coeff <- stats::glm.fit(cbind(difference, adj)[sbst, , drop=FALSE],
                              dat.tmp$winsA, family = wl.glm$family, control = wl.glm$control)$coefficients
      ftd[grp2 == i] <- apply(cbind(difference, adj)[grp2 == i, , drop=FALSE], 1, function(x) sum(x * coeff, na.rm = TRUE))
    }
    out$running.values <- wl.glm$family$linkinv(ftd)
  }

  structure(out, class = c(if(running) "elo.running", "elo.winloss"))
}

#' @export
print.elo.winloss <- function(x, ...)
{
  cat("\nAn object of class 'elo.winloss', containing information on ", length(x$teams),
      " teams and ", sum(x$n.games)/2, " matches.\n\n", sep = "")
  invisible(x)
}
