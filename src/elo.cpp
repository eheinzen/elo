#include <Rcpp.h>
using namespace Rcpp;

double eloProb(double eloA, double eloB)
{
  return 1/(1 + exp(log(10.0)*(eloB - eloA)/400.0));
}

double eloUpdate(double prob, double winsA, double k)
{
  return k*(winsA - prob);
}

NumericVector eloRegress(NumericVector eloA, NumericVector to, double by, LogicalVector idx)
{
  for(int i = 0; i < eloA.size(); i++)
  {
    if(idx[i])
    {
      eloA[i] = eloA[i] + by*(to[i] - eloA[i]);
    }
  }

  return eloA;
}

// [[Rcpp::export]]
List eloRun(NumericMatrix teamA, NumericMatrix teamB, NumericVector weightsA, NumericVector weightsB,
            NumericVector winsA, NumericMatrix k, NumericVector adjTeamA, NumericVector adjTeamB,
            LogicalVector regress, NumericVector to, double by, bool regressUnused,
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

  NumericMatrix out(nGames, 4 + 2*nBoth);
  NumericMatrix regOut(nRegress, nTeams);

  int regRow = 0;
  for(int i = 0; i < nGames; i++)
  {
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
    for(int l = 0; l < ncolB; l++)
    {
      if(flag == 2)
      {
        e2[l] = teamB(i, l);
        out(i, ncolA + l) = 0;
      } else
      {
        double tmB = teamB(i, l);
        e2[l] = currElo[tmB];
        usedYet[tmB] = true;
        out(i, ncolA + l) = tmB + 1;
      }
    }

    // calculate and store the update
    double prb = eloProb(sum(e1) + adjTeamA[i], sum(e2) + adjTeamB[i]);
    double updt1 = eloUpdate(prb, winsA[i], k(i, 0));
    double updt2 = eloUpdate(prb, winsA[i], k(i, 1)) * -1.0;

    out(i, nBoth) = prb;
    out(i, nBoth + 1) = winsA[i];
    out(i, nBoth + 2) = updt1;
    out(i, nBoth + 3) = updt2;

    // store new Elos for team A
    for(int j = 0; j < ncolA; j++)
    {
      double tmp = e1[j] + updt1 * weightsA[j];
      out(i, nBoth + 4 + j) = tmp;
      currElo[teamA(i, j)] = tmp;
    }

    // store new Elos for team B
    for(int l = 0; l < ncolB; l++)
    {
      if(flag == 2)
      {
        out(i, nBoth + 4 + ncolA + l) = e2[l];
      } else
      {
        double tmp = e2[l] + updt2 * weightsB[l];
        out(i, nBoth + 4 + ncolA + l) = tmp;
        currElo[teamB(i, l)] = tmp;
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
        for(int l = 0; l < nTeams; l++)
        {
          usedYet[l] = false;
        }
      }
    }
  }

  return List::create(out, regOut);
}

