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

// [[Rcpp::export]]
NumericMatrix eloRun(NumericVector teamA, NumericVector teamB, NumericVector winsA,
                     NumericVector k, NumericVector adjTeamA, NumericVector adjTeamB,
                     NumericVector initialElos, int flag)
{
  int mult = 1 + (flag != 2);
  int nTeams = initialElos.size();
  int nGames = winsA.size();
  NumericVector currElo(nTeams);
  currElo = initialElos;
  NumericMatrix out(mult*nGames + nTeams, 5);
  double tmp = 0, prb = 0;
  int row = 0;
  double e1 = 0, e2 = 0, j1 = 0, j2 = 0;

  // Get the initial Elos in the first few spots
  for(int i = 0; i < nTeams; i++)
  {
    out(i, 0) = 0; // the zeroth game
    out(i, 1) = i; // the ith team
    out(i, 2) = currElo[i];
  }

  for(int i = 0; i < nGames; i++)
  {
    j1 = teamA[i];
    e1 = currElo[j1];

    if(flag == 2)
    {
      e2 = teamB[i];
    } else
    {
      j2 = teamB[i];
      e2 = currElo[j2];
    }
    prb = eloProb(e1 + adjTeamA[i], e2 + adjTeamB[i]);
    tmp = eloUpdate(e1 + adjTeamA[i], e2 + adjTeamB[i], winsA[i], k[i]);

    row = nTeams + mult*i;
    out(row, 0) = i + 1;
    out(row, 1) = j1;
    out(row, 2) = e1 + tmp;
    out(row, 3) = prb;
    out(row, 4) = winsA[i];
    currElo[j1] = e1 + tmp;

    if(flag != 2)
    {
      row = nTeams + mult*i + 1;
      out(row, 0) = i + 1;
      out(row, 1) = j2;
      out(row, 2) = e2 - tmp;
      out(row, 3) = 1 - prb;
      out(row, 4) = 1 - winsA[i];
      currElo[j2] = e2 - tmp;
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericMatrix eloRunAsMatrix(NumericMatrix mat)
{
  double nTeams = max(mat(_, 1)) + 1;
  double nGames = max(mat(_, 0)) + 1;
  NumericMatrix out(nGames, nTeams);
  int row = 0;
  int nRows = mat.nrow();

  for(int i = 0; i < nGames; i++)
  {
    if(i > 0)
    {
      out(i, _) = out(i-1, _);
    }

    do
    {
      out(i, mat(row, 1)) = mat(row, 2);
      row++;
    } while (row < nRows && mat(row, 0) == i);

  }
  return out;
}
