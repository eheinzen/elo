#' \code{elo.winpct}
#'
#' Compute a (usually logistic) regression based on win percentage for a matchup.
#'
#' @inheritParams elo.glm
#' @param weights A vector of weights. Note that these are used in calculating wins and losses but
#'   not in the regression.
#' @details
#' Win percentages are first calculated. Anything passed to \code{\link{adjust}()} in
#'   \code{formula} is also put in the data.frame. A \code{\link{glm}} model is then
#'   run to predict wins or margin of victory.
#'
#'   With this setup, the intercept represents the home-field advantage. Neutral fields can be indicated
#'   using the \code{\link{neutral}()} function, which sets the intercept to 0.
#' @examples
#' elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   subset = points.Home != points.Visitor)
#'
#' elo.winpct(mov(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   family = "gaussian")
#' @seealso \code{\link[stats]{glm}}, \code{\link{summary.elo.winpct}}, \code{\link{score}},
#'   \code{\link{mov}}, \code{\link{elo.model.frame}}
#' @name elo.winpct
NULL
#> NULL

#' @rdname elo.winpct
#' @export
elo.winpct <- function(formula, data, family = "binomial", weights, na.action, subset, ..., running = FALSE, skip = 0)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "neutral", "weights")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- check_elo_winpct_vars(mf)
  all.teams <- attr(dat, "teams")
  grp <- mf$group

  out <- do.call(eloWinPct, dat)
  vec <- stats::setNames(out[[1]], all.teams)

  difference <- mean_vec_subset_matrix(vec, dat$teamA+1) - mean_vec_subset_matrix(vec, dat$teamB+1)
  wl.dat <- data.frame(wins.A = mf$wins.A, home.field = mf$home.field, difference = difference)
  if(!all(mf$adj.A == 0)) wl.dat$adj.A <- mf$adj.A
  if(!all(mf$adj.B == 0)) wl.dat$adj.B <- mf$adj.B
  wl.glm <- stats::glm(wins.A ~ . - 1, family = family, data = wl.dat)
  out <- list(
    fit = wl.glm,
    weights = mf$weights,
    win.pct = stats::setNames(out[[1]], all.teams),
    n.games = out[[2]],
    y = wl.glm$y,
    fitted.values = wl.glm$fitted.values,
    teams = all.teams,
    group = grp,
    elo.terms = Terms,
    na.action = stats::na.action(mf),
    outcome = attr(mf, "outcome")
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
      dat.tmp[1:2] <- lapply(dat.tmp[1:2], `[`, sbst)
      dat.tmp$teamA <- dat.tmp$teamA[sbst, , drop = FALSE]
      dat.tmp$teamB <- dat.tmp$teamB[sbst, , drop = FALSE]

      wl <- do.call(eloWinPct, dat)
      vec <- stats::setNames(wl[[1]], all.teams)

      difference <- mean_vec_subset_matrix(vec, dat$teamA+1) - mean_vec_subset_matrix(vec, dat$teamB+1)

      # tmpfit <- stats::glm(dat$winsA ~ difference, subset = sbst, family = "binomial")
      # ftd[grp2 == i] <- predict(tmpfit, newdata = data.frame(difference = difference[grp2 == i]), type = "link")

      coeff <- stats::glm.fit(cbind(difference, adj)[sbst, , drop=FALSE],
                              dat.tmp$winsA, family = wl.glm$family, control = wl.glm$control)$coefficients
      ftd[grp2 == i] <- apply(cbind(difference, adj)[grp2 == i, , drop=FALSE], 1, function(x) sum(x * coeff, na.rm = TRUE))
    }
    out$running.values <- wl.glm$family$linkinv(ftd)
  }

  structure(out, class = c(if(running) "elo.running", "elo.winpct"))
}

#' @export
print.elo.winpct <- function(x, ...)
{
  cat("\nAn object of class 'elo.winpct', containing information on ", length(x$teams),
      " teams and ", sum(x$n.games)/2, " matches.\n\n", sep = "")
  invisible(x)
}
