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
  int nTeams = initialElos.size();
  int nGames = winsA.size();
  int nRegress = sum(regress);

  NumericVector currElo(nTeams);
  currElo = initialElos;

  NumericMatrix out(nGames, 7);
  NumericMatrix regOut(1 + nRegress, nTeams);
  regOut(0, _) = initialElos;

  double tmp = 0, prb = 0;
  int regRow = 1;
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
      currElo = eloRegress(currElo, to, by);
      regOut(regRow, _) = currElo;
      regRow++;
    }
  }

  return List::create(out, regOut);
}

// [[Rcpp::export]]
NumericMatrix eloRunAsMatrix(NumericMatrix mat, NumericMatrix regMat,
                             LogicalVector regress, LogicalVector group)
{
  // this function uses 1-based indexing, since the incoming matrix does, too
  double nTeams = regMat.ncol();
  double nGames = mat.nrow();
  double nOut = sum(group);
  NumericMatrix out(nOut, nTeams);
  NumericVector curr(nTeams);
  int regRow = 1, outRow = 0;

  for(int i = 0; i < nGames; i++)
  {
    if(i == 0)
    {
      curr = regMat(0, _);
    } else if(regress[i - 1])
    {
      curr = regMat(regRow, _);
      regRow++;
    }

    curr(mat(i, 0) - 1) = mat(i, 5);
    if(mat(i, 1) > 0)
    {
      curr(mat(i, 1) - 1) = mat(i, 6);
    }

    if(group[i])
    {
      out(outRow, _) = curr;
      outRow++;
    }
  }
  return out;
}

bool anyZero(NumericVector x)
{
  for(int i = 0; i < x.size(); i++)
  {
    if(x[i] == 0)
    {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
NumericVector finalElos(NumericMatrix mat, int nTeams)
{
  NumericVector out(nTeams);
  int t1 = 0, t2 = 0;
  for(int row = mat.nrow() - 1; row > -1; row--)
  {
    t1 = mat(row, 0) - 1;
    t2 = mat(row, 1) - 1;
    if(out[t1] == 0)
    {
      out[t1] = mat(row, 5);
    }
    if(t2 >= 0 && out[t2] == 0)
    {
      out[t2] = mat(row, 6);
    }

    if(!anyZero(out))
    {
      return out;
    }
  }
  return out;
}



