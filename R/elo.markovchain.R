#' \code{elo.markovchain}
#'
#' Compute a Markov chain model for a matchup.
#'
#' @inheritParams elo.glm
#' @param weights A vector of weights.
#' @param k A vector of probabilities. See details.
#' @examples
#' elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
#'   subset = points.Home != points.Visitor, k = 0.7)
#' @name elo.markovchain
NULL
#> NULL

#' @rdname elo.markovchain
#' @export
elo.markovchain <- function(formula, data, weights, na.action, subset, k = NULL, ..., running = FALSE, skip = 0)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "weights", "k")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  dat <- check_elo_markovchain_vars(mf)
  all.teams <- attr(dat, "teams")
  grp <- mf$group

  idx <- dat$winsA %in% 0:1
  if(!all(idx)) warning("Removing wins not in c(0, 1)")
  grp <- grp[idx]
  dat <- dat[idx, , drop = FALSE]

  # we use the convention Ax = x
  out <- do.call(eloMarkovChain, c(as.list(dat), list(nTeams = length(all.teams))))
  if(any(abs(colSums(out[[1]]) - 1) > sqrt(.Machine$double.eps))) warning("colSums(transition matrix) may not be 1")

  eig <- eigen(out[[1]])
  vec <- as.numeric(eig$vectors[, 1])
  vec <- stats::setNames(vec / sum(vec), all.teams)
  val <- as.numeric(eig$values[1])

  mc.glm <- stats::glm(wins.A ~ diff, data = data.frame(wins.A = dat$winsA, diff = vec[dat$teamA+1] - vec[dat$teamB+1]), family = "binomial")

  structure(list(
    transition = out[[1]],
    n.games = out[[2]],
    pi = vec,
    eigenvalue = val,
    fit = mc.glm,
    na.action = stats::na.action(mf),
    terms = Terms,
    teams = all.teams
  ), class = c(if(running) "elo.markovchain.running", "elo.markovchain"))
}

#' @export
print.elo.markovchain <- function(x, ...)
{
  cat("\nAn object of class 'elo.markovchain', containing information on ", length(x$teams),
      " teams and ", sum(x$n.games)/2, " matches.\n\n", sep = "")
  invisible(x)
}
