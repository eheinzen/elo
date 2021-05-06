
#' Compute win probabilities based on the beta distribution
#'
#' @inheritParams elo.calc
#' @param initial.alphas,initial.betas Initial values. Default is 1 (akin to a uniform distribution).
#' @export
elo.beta <- function(formula, data, na.action, subset, initial.alphas = NULL, initial.betas = initial.alphas, ...)
{
  Call <- match.call()
  Call <- Call[c(1, match(c("formula", "data", "subset", "na.action"), names(Call), nomatch = 0))]
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "group", "regress")
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  checked <- check_elo_beta_vars(mf, initial.alphas = initial.alphas, initial.betas = initial.betas)
  out <- do.call(eloBeta, checked)

  any.regr <- any(checked$regress)

  structure(list(
    results = out[[1]],
    n.matches = nrow(out[[1]]),
    n.players = c(ncol(checked$teamA), ncol(checked$teamB)),
    initial.alphas = checked$initialAlphas,
    initial.betas = checked$initialBetas,
    results.regressed = if(any.regr) out[[2]] else NULL,
    teams = names(checked$initialAlphas),
    group = mf$group,
    regress = if(any.regr) mf$regress else NULL,
    terms = Terms,
    na.action = stats::na.action(mf)
  ), class = c(if(any.regr) "elo.beta.regressed", "elo.beta"))
}



#' @export
print.elo.beta <- function(x, ...)
{
  cat("\nAn object of class '", class(x)[1], "', containing information on ",
      length(x$teams), " teams and ", x$n.matches, " matches.\n\n", sep = "")
  invisible(x)
}


#' @export
print.elo.beta.regressed <- function(x, ...)
{
  cat("\nAn object of class '", class(x)[1], "', containing information on ",
      length(x$teams), " teams and ", x$n.matches, " matches, with ",
      nrow(x$results.regressed), " regressions.\n\n", sep = "")
  invisible(x)
}


check_elo_beta_vars <- function(mf, initial.alphas = NULL, initial.betas = NULL)
{
  t1 <- mf$elo.A
  t2 <- mf$elo.B

  if(is.numeric(t1) || is.numeric(t2)) stop("Neither team should be numeric")
  if(!is.players(t1)) t1 <- players(t1)
  if(anyNA(t1)) stop("NAs were found in team.A; check that it can be coerced to character.")

  if(!is.players(t2)) t2 <- players(t2)
  if(anyNA(t2)) stop("NAs were found in team.B; check that it can be coerced to character.")
  all.teams <- sort(unique(c(as.character(t1), as.character(t2))))

  tmp <- stats::setNames(seq_along(all.teams) - 1L, all.teams)
  wts1 <- weights(t1)
  wts2 <- weights(t2)
  t1 <- matrix(tmp[t1], nrow = nrow(t1))
  t2 <- matrix(tmp[t2], nrow = nrow(t2))

  initial.alphas <- check_named_elos(initial.alphas, all.teams, default = 1, what = "alphas")
  initial.betas <- check_named_elos(initial.betas, all.teams, default = 1, what = "betas")

  regress <- check_group_regress(mf$regress)
  if(NCOL(to <- attr(mf$regress, "to")) == 2)
  {
    toAlpha <- to[, 1, drop = TRUE]
    toBeta <- to[, 2, drop = TRUE]
  } else toAlpha <- toBeta <- to
  toAlpha <- check_named_elos(toAlpha, all.teams)
  toBeta <- check_named_elos(toBeta, all.teams)

  group <- check_group_regress(mf$group, gt.zero = TRUE)
  if(any(regress & !group)) stop("You can't regress mid-group")

  list(winsA = mf$wins.A, teamA = t1, teamB = t2, weightsA = wts1, weightsB = wts2,
       adjTeamA = mf$adj.A, adjTeamB = mf$adj.B, regress = regress,
       toAlpha = toAlpha, toBeta = toBeta, by = attr(mf$regress, "by"),
       regressUnused = attr(mf$regress, "regress.unused"),
       group = group,
       initialAlphas = initial.alphas, initialBetas = initial.betas)
}


eloBeta <- function(teamA, teamB, weightsA, weightsB, winsA, k, adjTeamA, adjTeamB, regress, toAlpha, toBeta, by, regressUnused, group, initialAlphas, initialBetas)
{
  eloRegress <- function(eloA, to, by, idx) ifelse(idx, eloA + by*(to - eloA), eloA)
  betaProb <- function(alpha1, beta1, alpha2, beta2, adjust.A, adjust.B, wtsA, wtsB)
  {
    f <- function(x) sum(wtsA*pbeta(x, alpha2 + adjust.B, beta2))*sum(wtsB*dbeta(x, alpha1 + adjust.A, beta1))
    f2 <- function(y) vapply(y, f, NA_real_)
    integrate(f2, lower = 0, upper = 1)$value
  }

  nTeams <- length(initialAlphas)
  ncolA <- ncol(teamA)
  ncolB <- ncol(teamB)
  nBoth <- ncolA + ncolB
  nGames <- length(winsA)
  nRegress <- sum(regress)

  usedYet <- logical(nTeams)
  currAlphas <- initialAlphas # R automatically deep copies
  currBetas <- initialBetas # R automatically deep copies
  groupAlphas <- initialAlphas # R automatically deep copies
  groupBetas <- initialBetas # R automatically deep copies

  out <- matrix(0, nrow = nGames, ncol = 4 + 3*nBoth)
  regOut <- matrix(0, nrow = nRegress, ncol = 2*nTeams)

  regRow <- 1
  for(i in seq_len(nGames))
  {
    a1 <- numeric(ncolA)
    a2 <- numeric(ncolB)
    b1 <- numeric(ncolA)
    b2 <- numeric(ncolB)
    currA1 <- numeric(ncolA)
    currA2 <- numeric(ncolB)
    currB1 <- numeric(ncolA)
    currB2 <- numeric(ncolB)

    # get initial Elos for team A
    for(j in seq_len(ncolA))
    {
      tmA <- teamA[i, j] + 1
      a1[j] <- groupAlphas[tmA]
      b1[j] <- groupBetas[tmA]
      currA1[j] <- currAlphas[tmA]
      currB1[j] <- currBetas[tmA]
      usedYet[tmA] <- TRUE
      out[i, j] <- tmA
    }

    # get initial Elos for team B
    for(l in seq_len(ncolB))
    {
      tmB <- teamB[i, l] + 1
      a2[l] <- groupAlphas[tmB]
      b2[l] <- groupBetas[tmB]
      currA2[l] <- currAlphas[tmB]
      currB2[l] <- currBetas[tmB]
      usedYet[tmB] <- TRUE
      out[i, ncolA + l] <- tmB
    }

    # calculate and store the update
    prb <- betaProb(alpha1 = a1, beta1 = b1, alpha2 = a2, beta2 = b2, adjust.A = adjTeamA[i], adjust.B = adjTeamB[i], wtsA = weightsA, wtsB = weightsB)
    updt1 <- max(winsA[i] - prb, 0)
    updt2 <- max(prb - winsA[i], 0)

    out[i, nBoth + 1] <- prb
    out[i, nBoth + 2] <- winsA[i]
    out[i, nBoth + 3] <- updt1 # alpha1/beta2
    out[i, nBoth + 4] <- updt2 # beta1/alpha2

    # store new values for team A
    for(j in seq_len(ncolA))
    {
      tmp <- currA1[j] + updt1 * weightsA[j]
      out[i, nBoth + 4 + j] <- tmp
      currAlphas[teamA[i, j] + 1] <- tmp

      tmp <- currB1[j] + updt2 * weightsA[j]
      out[i, 2*nBoth + 4 + j] <- tmp
      currBetas[teamA[i, j] + 1] <- tmp
    }

    # store new values for team B
    for(l in seq_len(ncolB))
    {
      tmp <- currA2[l] + updt2 * weightsB[l]
      out[i, nBoth + 4 + ncolA + l] <- tmp
      currAlphas[teamB[i, l] + 1] <- tmp

      tmp <- currB2[l] + updt1 * weightsB[l]
      out[i, 2*nBoth + 4 + ncolA + l] <- tmp
      currBetas[teamB[i, l] + 1] <- tmp
    }

    # This part is fine
    if(regress[i])
    {
      currAlphas <- eloRegress(currAlphas, toAlpha, by, usedYet)
      regOut[regRow, seq_len(nTeams)] <- currAlphas
      currBetas <- eloRegress(currBetas, toBeta, by, usedYet)
      regOut[regRow, nTeams + seq_len(nTeams)] <- currBetas

      regRow <- regRow + 1
      if(!regressUnused)
      {
        for(l in seq_len(nTeams))
        {
          usedYet[l] <- FALSE
        }
      }
    }

    if(group[i])
    {
      groupAlphas = currAlphas;
      groupBetas = currBetas;
    }
  }

  list(out, regOut)
}

