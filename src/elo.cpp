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

double eloUpdate2(double prob, double winsA, double k)
{
  return k*(winsA - prob);
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
List eloRun(NumericMatrix teamA, NumericMatrix teamB, NumericVector weightsA, NumericVector weightsB,
            NumericVector winsA, NumericVector k, NumericVector adjTeamA, NumericVector adjTeamB,
            LogicalVector regress, double to, double by, bool regressUnused,
            NumericVector initialElos, int flag)
{
  // this function uses 0-based indexing, since the incoming vectors used -1L
  int nTeams = initialElos.size();
  int ncolA = teamA.ncol();
  int ncolB = teamB.ncol();
  int nBoth = ncolA + ncolB;
  int nGames = winsA.size();
  int nRegress = sum(regress);

  NumericVector currElo(nTeams);
  LogicalVector usedYet(nTeams);
  currElo = clone(initialElos);

  NumericMatrix out(nGames, 3 + 2*nBoth);
  NumericMatrix regOut(nRegress, nTeams);

  int regRow = 0;
  for(int i = 0; i < nGames; i++)
  {
    NumericVector t1(ncolA);
    NumericVector t2(ncolB);
    NumericVector e1(ncolA);
    NumericVector e2(ncolB);

    // get initial Elos for team A
    for(int j = 0; j < ncolA; j++)
    {
      double tmA = teamA(i, j);
      e1[j] = currElo[tmA];
      usedYet[tmA] = true;
      out(i, j) = tmA + 1;
    }

    // get initial Elos for team B
    for(int k = 0; k < ncolB; k++)
    {
      if(flag == 2)
      {
        e2[k] = teamB(i, k);
        out(i, ncolA + k) = 0;
      } else
      {
        double tmB = teamB(i, k);
        e2[k] = currElo[tmB];
        usedYet[tmB] = true;
        out(i, ncolA + k) = tmB + 1;
      }
    }

    // calculate and store the update
    double prb = eloProb(sum(e1) + adjTeamA[i], sum(e2) + adjTeamB[i]);
    double updt = eloUpdate2(prb, winsA[i], k[i]);

    out(i, nBoth) = prb;
    out(i, nBoth + 1) = winsA[i];
    out(i, nBoth + 2) = updt;

    // store new Elos for team A
    for(int j = 0; j < ncolA; j++)
    {
      double tmp = e1[j] + updt * weightsA[j];
      out(i, nBoth + 3 + j) = tmp;
      currElo[teamA(i, j)] = tmp;
    }

    // store new Elos for team B
    for(int k = 0; k < ncolB; k++)
    {
      if(flag == 2)
      {
        out(i, nBoth + 3 + ncolA + k) = e2[k];
      } else
      {
        double tmp = e2[k] - updt * weightsB[k];
        out(i, nBoth + 3 + ncolA + k) = tmp;
        currElo[teamB(i, k)] = tmp;
      }
    }

    // This part is fine
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

