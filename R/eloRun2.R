eloRun2 <- function(teamA, teamB, weightsA, weightsB, winsA, k, adjTeamA, adjTeamB, regress, to, by, regressUnused, group, initialElos, flag, prob.fun, update.fun)
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
  groupElo <- numeric(nTeams)
  groupElo <- initialElos # R automatically deep copies

  out <- matrix(0, nrow = nGames, ncol = 4 + 2*nBoth)
  regOut <- matrix(0, nrow = nRegress, ncol = nTeams)

  regRow <- 1
  for(i in seq_len(nGames))
  {
    e1 <- numeric(ncolA)
    e2 <- numeric(ncolB)
    curr1 <- numeric(ncolA)
    curr2 <- numeric(ncolB)

    # get initial Elos for team A
    for(j in seq_len(ncolA))
    {
      tmA <- teamA[i, j] + 1
      e1[j] <- groupElo[tmA]
      curr1[j] <- currElo[tmA]
      usedYet[tmA] <- TRUE
      out[i, j] <- tmA
    }

    # get initial Elos for team B
    for(l in seq_len(ncolB))
    {
      if(flag == 2)
      {
        e2[l] <- teamB[i, l]
        curr2[l] <- teamB[i, l]
        out[i, ncolA + l] <- 0
      } else
      {
        tmB <- teamB[i, l] + 1
        e2[l] <- groupElo[tmB]
        curr2[l] <- currElo[tmB]
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
      tmp <- curr1[j] + updt1 * weightsA[j]
      out[i, nBoth + 4 + j] <- tmp
      currElo[teamA[i, j] + 1] <- tmp
    }

    # store new Elos for team B
    for(l in seq_len(ncolB))
    {
      if(flag == 2)
      {
        out[i, nBoth + 4 + ncolA + l] <- curr2[l]
      } else
      {
        tmp <- curr2[l] + updt2 * weightsB[l]
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

    if(group[i])
    {
      groupElo = currElo;
    }
  }

  list(out, regOut)
}

