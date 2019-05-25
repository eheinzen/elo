
#' \code{elo.glm}
#'
#' Compute a logistic regression model for a matchup.
#'
#' @inheritParams elo.calc
#' @param family,weights,... Arguments passed to \code{\link[stats]{glm}}.
#' @param rm.ties Logical, denoting whether to remove ties on the left-hand side.
#' @param running Logical, denoting whether to calculate "running" projected probabilities. If true, a model is fit for
#'   group 1 on its own to predict group 2, then groups 1 and 2 to predict 3, then groups 1 through 3 to predict 4, etc.
#'   Groups are determined in \code{formula}. Omitting a group term re-runs a glm model to predict each
#'   observation (a potentially time-consuming operation!)
#' @param skip Integer, denoting how many groups to skip before fitting the running models. This is helpful if
#'   groups are small, where glm would have trouble converging for the first few groups. The predicted values are then
#'   set to 0.5 for the skipped groups.
#' @return An object of class \code{c("elo.glm", "glm")}. If \code{running==TRUE}, the class \code{"elo.glm.running"}
#'   is prepended.
#' @details
#'   The formula syntax is the same as other \code{elo} functions. A data.frame
#'   of indicator variables is built, where an entry is 1 if a team is home, 0 if
#'   a team didn't play, and -1 if a team is a visitor. Anything passed to \code{\link{adjust}()} in
#'   \code{formula} is also put in the data.frame. A \code{\link{glm}} model is then
#'   run to predict wins.
#'
#'   With this setup, the intercept represents the home-field advantage. Neutral fields can be indicated
#'   using the \code{\link{adjust}()} function.
#' @examples
#' data(tournament)
#' elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament)
#'
#' @seealso \code{\link[stats]{glm}}, \code{\link{summary.elo.glm}}, \code{\link{score}}, \code{\link{elo.model.frame}}
#' @name elo.glm
NULL
#> NULL

#' @rdname elo.glm
#' @export
elo.glm <- function(formula, data, weights, na.action, subset, family = "binomial", ..., rm.ties = FALSE, running = FALSE, skip = 0)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "weights")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- mf_to_wide(mf)
  all.teams <- attr(dat, "all.teams")

  # find spanning set
  QR <- qr(cbind(intercept = 1, dat))
  dat <- dat[QR$pivot[seq_len(QR$rank)] - 1]

  dat$wins.A <- mf$wins.A
  grp <- mf$group
  if(rm.ties)
  {
    idx <- dat$wins.A %in% 0:1
    grp <- grp[idx]
    dat <- dat[idx, , drop = FALSE]
  }

  wts <- mf$weights
  dat.glm <- stats::glm(wins.A ~ ., data = dat, family = family, na.action = stats::na.pass, subset = NULL, weights = wts, ...)
  dat.glm$teams <- all.teams
  dat.glm$group <- grp
  dat.glm$elo.terms <- Terms
  dat.glm$rm.ties <- rm.ties
  dat.glm$na.action <- stats::na.action(mf)

  if(running)
  {
    dat.mat <- cbind(1, as.matrix(dat[names(dat) != "wins.A"]))
    y <- dat$wins.A

    ftd <- rep(0, times = nrow(dat))
    grp2 <- group_to_int(grp, skip)

    for(i in setdiff(seq_len(max(grp2)), seq_len(skip)))
    {
      if(i == 1) next
      sbst <- grp2 %in% 1:(i-1)

      # tmpfit <- stats::glm(wins.A ~ ., data = dat, subset = sbst, weights = wts, family = family)
      # ftd[grp2 == i] <- predict(tmpfit, newdata = dat[grp2 == i, ], type = "link")

      coeff <- stats::glm.fit(dat.mat[sbst, , drop = FALSE], y[sbst], wts[sbst], family = dat.glm$family,
                                control = dat.glm$control)$coefficients
      ftd[grp2 == i] <- apply(dat.mat[grp2 == i, , drop = FALSE], 1, function(x) sum(x * coeff))
    }
    dat.glm$running.values <- dat.glm$family$linkinv(ftd)
  }

  structure(dat.glm, class = c(if(running) "elo.glm.running", "elo.glm", class(dat.glm)))
}
