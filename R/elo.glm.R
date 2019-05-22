
#' \code{elo.glm}
#'
#' Compute a logistic regression model for a matchup.
#'
#' @inheritParams elo.calc
#' @param family,weights,... Arguments passed to \code{\link[stats]{glm}}.
#' @param rm.ties Logical, denoting whether to remove ties on the left-hand side.
#' @param running Logical, denoting whether to calculate "running" probabilities. If true, a model is fit for
#'   group 1 on its own, then groups 1 and 2, then groups 1 through 3, etc. Groups are determined
#'   in \code{formula}. Omitting a group term re-runs a glm model for each observation (a potentially
#'   time-consuming operation!)
#' @param skip Integer, denoting how many groups to skip before fitting the running models. This is helpful if
#'   groups are small, where glm would have trouble converging for the first few groups.
#' @return An object of class \code{c("elo.glm", "glm")}. If \code{running==TRUE}, the class \code{"elo.glm.running"}
#'   is prepended.
#' @details
#'   The formula syntax is the same as other \code{elo} functions. A data.frame
#'   of indicator variables is built, where an entry is 1 if a team is home, 0 if
#'   a team didn't play, and -1 if a team is a visitor. A \code{\link{glm}} model is then
#'   run to predict wins.
#'
#'   With this setup, usually one model term will be NA, as the input is linearly dependent.
#'   Consider this team's skill to be zero relative to the other teams.
#'   The intercept represents the home-field advantage.
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
  Terms <- stats::terms(mf)

  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")

  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))

  dat <- lapply(all.teams, function(tm) (rowSums(t1 == tm) > 0) - (rowSums(t2 == tm) > 0))
  names(dat) <- all.teams
  dat$wins.A <- mf$wins.A
  dat <- structure(dat, class = "data.frame", row.names = c(NA_integer_, nrow(mf)))
  grp <- mf$group
  if(rm.ties)
  {
    idx <- dat$wins.A %in% 0:1
    grp <- grp[idx]
    dat <- dat[idx, , drop = FALSE]
  }

  wts <- mf$weights
  dat.glm <- stats::glm(wins.A ~ ., data = dat, family = family, na.action = stats::na.pass, subset = NULL, weights = wts, ...)
  dat.glm$na.action <- stats::na.action(mf)

  if(running)
  {
    dat.mat <- cbind(1, as.matrix(dat[names(dat) != "wins.A"]))
    y <- dat$wins.A
    if(is.null(wts)) wts <- rep(1, nrow(dat.mat))

    ftd <- dat.glm$fitted.values
    grp2 <- check_group_regress(grp, gt.zero = FALSE)
    grp2 <- rev(cumsum(rev(grp2)))
    mx <- max(grp2)
    if(skip > mx || skip < 0) stop("skip must be between 0 and ", mx, " (inclusive)")
    grp2 <- mx + 1 - grp2 # from mx : 1 to 1 : mx
    for(i in rev(setdiff(seq_len(mx-1), seq_len(skip))))
    {
      # we're looping over the groups in reverse, excluding the last group, whose estimates we already have from dat.glm
      sbst <- grp2 %in% 1:i
      # the "<=" here assigns the final fitted values from i=skip+1 to the skipped i <= skip
      ftd[grp2 <= i] <- stats::glm.fit(dat.mat[sbst, , drop = FALSE], y[sbst], wts[sbst], family = dat.glm$family,
                                control = dat.glm$control)$fitted.values[grp2 <= i]
    }
    dat.glm$running.fitted.values <- ftd
  }

  structure(dat.glm, class = c(if(running) "elo.glm.running", "elo.glm", class(dat.glm)),
            rm.ties = rm.ties, all.teams = all.teams)
}
