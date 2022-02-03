#' Compute a (usually logistic) regression model for a series of matches.
#'
#' @inheritParams elo.calc
#' @param family,weights,... Arguments passed to \code{\link[stats]{glm}}.
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
#'   run to predict wins or margin of victory.
#'
#'   With this setup, the intercept represents the home-field advantage. Neutral fields can be indicated
#'   using the \code{\link{neutral}()} function, which sets the intercept to 0.
#'
#'   Note that any weights specified in \code{players()} will be ignored.
#'
#'   This is essentially the Bradley-Terry model.
#' @references https://en.wikipedia.org/wiki/Bradley%E2%80%93Terry_model
#' @examples
#' data(tournament)
#' elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   subset = points.Home != points.Visitor)
#' elo.glm(mov(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   family = "gaussian")
#' @seealso \code{\link[stats]{glm}}, \code{\link{summary.elo.glm}}, \code{\link{score}},
#'   \code{\link{mov}}, \code{\link{elo.model.frame}}
#' @name elo.glm
NULL
#> NULL

#' @rdname elo.glm
#' @export
elo.glm <- function(formula, data, family = "binomial", weights, na.action, subset, ..., running = FALSE, skip = 0)
{
  Call <- match.call()
  Call <- Call[c(1, match(c("formula", "data", "weights", "subset", "na.action"), names(Call), nomatch = 0))]
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "neutral", "weights")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- mf_to_wide(mf)
  all.teams <- attr(dat, "all.teams")

  # find spanning set
  QR <- qr(dat)
  dat.qr <- dat[QR$pivot[seq_len(QR$rank)]]

  dat.qr$wins.A <- mf$wins.A
  grp <- mf$group

  wts <- mf$weights
  out <- stats::glm(wins.A ~ . - 1, data = dat.qr, family = family, na.action = stats::na.pass, subset = NULL, weights = wts, ...)
  out$teams <- all.teams
  out$group <- grp
  out$elo.terms <- Terms
  out$na.action <- stats::na.action(mf)
  out$outcome <- attr(mf, "outcome")

  if(running)
  {
    dat.mat <- as.matrix(dat)
    y <- mf$wins.A

    ftd <- rep(0, times = nrow(dat))
    grp2 <- group_to_int(grp, skip)

    for(i in setdiff(seq_len(max(grp2)), seq_len(skip)))
    {
      if(i == 0) next
      sbst <- grp2 %in% 0:(i-1)

      # tmpfit <- stats::glm(wins.A ~ . - 1, data = dat, subset = sbst, weights = wts, family = family)
      # ftd[grp2 == i] <- predict(tmpfit, newdata = dat[grp2 == i, ], type = "link")

      d <- dat.mat[sbst, , drop = FALSE]
      coeff <- stats::glm.fit(d, y[sbst], wts[sbst], family = out$family,
                              control = out$control)$coefficients
      valid <- colSums(d != 0) > 0
      ftd[grp2 == i] <- apply(dat.mat[grp2 == i, , drop = FALSE], 1, mult_valid_coef, coeff = coeff, valid = valid)
    }
    out$running.values <- out$family$linkinv(ftd)
    attr(out$running.values, "group") <- grp2
  }

  structure(out, class = c(if(running) "elo.running", "elo.glm", class(out)))
}
