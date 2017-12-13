#include <Rcpp.h>
using namespace Rcpp;

double eloProb(double eloA, double eloB)
{
  return 1/(1 + exp(log(10.0)*(eloB - eloA)/400.0));
}

double eloUpdate(double eloA, double eloB, double winsA, double k)
{
  return k*(winsA - eloProb(eloA, eloB));
}

NumericVector eloRegress(NumericVector eloA, double to, double by, LogicalVector idx)
{
  for(int i = 0; i < eloA.size(); i++)
  {
    if(idx[i])
    {
      eloA[i] = eloA[i] + by*(to - eloA[i]);
    }
  }

  return eloA;
}

// [[Rcpp::export]]
List eloRun(NumericVector teamA, NumericVector teamB, NumericVector winsA,
                     NumericVector k, NumericVector adjTeamA, NumericVector adjTeamB,
                     LogicalVector regress, double to, double by, bool regressUnused,
                     NumericVector initialElos, int flag)
{
  // this function uses 0-based indexing, since the incoming vectors used -1L
  int nTeams = initialElos.size();
  int nGames = winsA.size();
  int nRegress = sum(regress);

  NumericVector currElo(nTeams);
  LogicalVector usedYet(nTeams);
  currElo = clone(initialElos);

  NumericMatrix out(nGames, 7);
  NumericMatrix regOut(nRegress, nTeams);

  double tmp = 0, prb = 0;
  int regRow = 0;
  double e1 = 0, e2 = 0, j1 = 0, j2 = 0;

  for(int i = 0; i < nGames; i++)
  {
    j1 = teamA[i];
    e1 = currElo[j1];
    usedYet[j1] = true;

    if(flag == 2)
    {
      e2 = teamB[i];
    } else
    {
      j2 = teamB[i];
      e2 = currElo[j2];
      usedYet[j2] = true;
    }
    prb = eloProb(e1 + adjTeamA[i], e2 + adjTeamB[i]);
    tmp = eloUpdate(e1 + adjTeamA[i], e2 + adjTeamB[i], winsA[i], k[i]);

    out(i, 0) = j1 + 1;
    out(i, 2) = prb;
    out(i, 3) = winsA[i];
    out(i, 4) = tmp;
    out(i, 5) = e1 + tmp;
    currElo[j1] = e1 + tmp;

    if(flag == 2)
    {
      out(i, 1) = 0;
      out(i, 6) = e2;
    } else
    {
      out(i, 1) = j2 + 1;
      out(i, 6) = e2 - tmp;
      currElo[j2] = e2 - tmp;
    }

    if(regress[i])
    {
      currElo = eloRegress(currElo, to, by, usedYet);
      regOut(regRow, _) = currElo;
      regRow++;
      if(!regressUnused)
      {
        for(int k = 0; k < nTeams; k++)
        {
          usedYet[k] = false;
        }
      }
    }
  }

  return List::create(out, regOut);
}
