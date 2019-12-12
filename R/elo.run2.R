#' @rdname elo.run
#' @param prob.fun A function with at least 4 arguments: elo.A, elo.B, adjust.A, and adjust.B. It should return a predicted probability
#'   that team A wins. The values passed in will be scalars, and a scalar is expected as output.
#' @param update.fun A function with at least 6 arguments: the same as \code{\link{elo.update.default}}. The function takes
#'   in the Elos, the win indicator, k, and any adjustments, and returns a value by which to update the Elos. The values passed in
#'   will be scalars, and a scalar is expected as output.
#' @details
#'   \code{elo.run} and \code{elo.run2} by default return the exact same thing. \code{elo.run} uses C++ and may be up to 50 times faster,
#'   while \code{elo.run2} uses R but also supports custom update functions. Prefer the first unless you really need a custom update function.
#' @export
elo.run2 <- function(formula, data, na.action, subset, k = NULL, initial.elos = NULL, ..., prob.fun = elo.prob, update.fun = elo.update)
{
  Call <- match.call()
  Call[[1L]] <- quote(elo::elo.model.frame)
  Call$required.vars <- c("wins", "elos", "k", "group", "regress")
  Call$ncol.k <- 2
  mf <- eval(Call, parent.frame())
  if(nrow(mf) == 0) stop("No (non-missing) observations")
  Terms <- stats::terms(mf)

  checked <- check_elo_run_vars(mf, initial.elos)
  checked$prob.fun <- match.fun(prob.fun)
  checked$update.fun <- match.fun(update.fun)
  out <- do.call(eloRun2, checked)
  any.regr <- any(checked$regress)

  structure(list(
    elos = out[[1]],
    n.players = c(ncol(checked$teamA), ncol(checked$teamB)),
    initial.elos = checked$initialElos,
    elos.regressed = if(any.regr) out[[2]] else NULL,
    teams = names(checked$initialElos),
    group = mf$group,
    regress = if(any.regr) mf$regress else NULL,
    terms = Terms,
    na.action = stats::na.action(mf)
  ), class = c(if(any.regr) "elo.run.regressed", "elo.run"))
}

eloRun2 <- function(teamA, teamB, weightsA, weightsB, winsA, k, adjTeamA, adjTeamB, regress, to, by, regressUnused, initialElos, flag, prob.fun, update.fun)
{
  eloRegress <- function(eloA, to, by, idx) ifelse(idx, eloA + by*(to - eloA), eloA)

  nTeams <- length(initialElos)
  ncolA <- ncol(teamA)
  ncolB <- ncol(teamB)
  nBoth <- ncolA + ncolB
  nGames <- length(winsA)
  nRegress <- sum(regress)

  currElo <- numeric(nTeams)
  usedYet <- logical(nTeams)
  currElo <- initialElos # R automatically deep copies

  out <- matrix(0, nrow = nGames, ncol = 4 + 2*nBoth)
  regOut <- matrix(0, nrow = nRegress, ncol = nTeams)

  regRow <- 1
  for(i in seq_len(nGames))
  {
    e1 <- numeric(ncolA)
    e2 <- numeric(ncolB)

    # get initial Elos for team A
    for(j in seq_len(ncolA))
    {
      tmA <- teamA[i, j] + 1
      e1[j] <- currElo[tmA]
      usedYet[tmA] <- TRUE
      out[i, j] <- tmA
    }

    # get initial Elos for team B
    for(l in seq_len(ncolB))
    {
      if(flag == 2)
      {
        e2[l] <- teamB[i, l]
        out[i, ncolA + l] <- 0
      } else
      {
        tmB <- teamB[i, l] + 1
        e2[l] <- currElo[tmB]
        usedYet[tmB] <- TRUE
        out[i, ncolA + l] <- tmB
      }
    }

    # calculate and store the update
    prb <- prob.fun(elo.A = sum(e1), elo.B = sum(e2), adjust.A = adjTeamA[i], adjust.B = adjTeamB[i])
    updt1 <- update.fun(wins.A = winsA[i], elo.A = sum(e1), elo.B = sum(e2), k = k[i, 1], adjust.A = adjTeamA[i], adjust.B = adjTeamB[i])
    updt2 <- update.fun(wins.A = winsA[i], elo.A = sum(e1), elo.B = sum(e2), k = k[i, 2], adjust.A = adjTeamA[i], adjust.B = adjTeamB[i]) * -1

    out[i, nBoth + 1] <- prb
    out[i, nBoth + 2] <- winsA[i]
    out[i, nBoth + 3] <- updt1
    out[i, nBoth + 4] <- updt2

    # store new Elos for team A
    for(j in seq_len(ncolA))
    {
      tmp <- e1[j] + updt1 * weightsA[j]
      out[i, nBoth + 4 + j] <- tmp
      currElo[teamA[i, j] + 1] <- tmp
    }

    # store new Elos for team B
    for(l in seq_len(ncolB))
    {
      if(flag == 2)
      {
        out[i, nBoth + 4 + ncolA + l] <- e2[l]
      } else
      {
        tmp <- e2[l] + updt2 * weightsB[l]
        out[i, nBoth + 4 + ncolA + l] <- tmp
        currElo[teamB[i, l] + 1] <- tmp
      }
    }

    # This part is fine
    if(regress[i])
    {
      currElo <- eloRegress(currElo, to, by, usedYet)
      regOut[regRow, ] <- currElo
      regRow <- regRow + 1
      if(!regressUnused)
      {
        for(l in seq_len(nTeams))
        {
          usedYet[l] <- FALSE
        }
      }
    }
  }

  list(out, regOut)
}

