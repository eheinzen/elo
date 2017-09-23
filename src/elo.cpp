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

NumericVector eloRegress(NumericVector eloA, double to, double by)
{
  return eloA + by*(to - eloA);
}

// [[Rcpp::export]]
List eloRun(NumericVector teamA, NumericVector teamB, NumericVector winsA,
                     NumericVector k, NumericVector adjTeamA, NumericVector adjTeamB,
                     LogicalVector regress, double to, double by,
                     NumericVector initialElos, int flag)
{
  // this function uses 0-based indexing, since the incoming vectors used -1L
  int mult = 1 + (flag != 2);
  int nTeams = initialElos.size();
  int nGames = winsA.size();
  int nRegress = sum(regress);

  NumericVector currElo(nTeams);
  currElo = initialElos;

  NumericMatrix out(mult*nGames, 5);
  NumericMatrix regOut(1 + nRegress, nTeams);
  regOut(0, _) = initialElos;

  double tmp = 0, prb = 0;
  int row = 0, regRow = 1;
  double e1 = 0, e2 = 0, j1 = 0, j2 = 0;

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

    row = mult*i;
    out(row, 0) = i + 1;
    out(row, 1) = j1 + 1;
    out(row, 2) = e1 + tmp;
    out(row, 3) = prb;
    out(row, 4) = winsA[i];
    currElo[j1] = e1 + tmp;

    if(flag != 2)
    {
      row = mult*i + 1;
      out(row, 0) = i + 1;
      out(row, 1) = j2 + 1;
      out(row, 2) = e2 - tmp;
      out(row, 3) = 1 - prb;
      out(row, 4) = 1 - winsA[i];
      currElo[j2] = e2 - tmp;
    }

    if(regress[i])
    {
      regOut(i, _) = currElo;
      i++;
      currElo = eloRegress(currElo, to, by);
    }
  }

  return List::create(out, regOut);
}

// [[Rcpp::export]]
NumericMatrix eloRunAsMatrix(NumericMatrix mat, NumericVector initialElos)
{
  // this function uses 1-based indexing, since the incoming matrix is
  double nTeams = initialElos.size();
  double nGames = max(mat(_, 0));
  NumericMatrix out(nGames, nTeams);
  int row = 0;
  int nRows = mat.nrow();

  for(int i = 0; i < nGames; i++)
  {
    if(i == 0)
    {
      out(i, _) = initialElos;
    } else
    {
      out(i, _) = out(i-1, _);
    }

    do
    {
      out(i, mat(row, 1) - 1) = mat(row, 2);
      row++;
    } while (row < nRows && mat(row, 0) == i + 1);

  }
  return out;
}
