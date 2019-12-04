#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix eloRunAsMatrix(NumericMatrix mat, NumericVector initialElos, LogicalVector group)
{
  // this function uses 1-based indexing, since the incoming matrix does, too
  double nTeams = initialElos.size();
  double nGames = mat.nrow();
  double nOut = sum(group);
  int nBoth = (mat.ncol() - 4) / 2;
  NumericMatrix out(nOut, nTeams);
  NumericVector curr(nTeams);
  curr = clone(initialElos);
  int outRow = 0;

  for(int i = 0; i < nGames; i++)
  {
    for(int j = 0; j < nBoth; j++)
    {
      double tm = mat(i, j);
      if(tm > 0)
      {
        curr[tm - 1] = mat(i, nBoth + 4 + j);
      }
    }

    if(group[i])
    {
      out(outRow, _) = curr;
      outRow++;
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix eloRunRegressedAsMatrix(NumericMatrix mat, NumericVector initialElos,
                                      NumericMatrix regMat, LogicalVector regress, LogicalVector group)
{
  // this function uses 1-based indexing, since the incoming matrix does, too
  double nTeams = initialElos.size();
  double nGames = mat.nrow();
  double nOut = sum(group);
  int nBoth = (mat.ncol() - 4) / 2;
  NumericMatrix out(nOut, nTeams);
  NumericVector curr(nTeams);
  curr = clone(initialElos);
  int regRow = 0, outRow = 0;

  for(int i = 0; i < nGames; i++)
  {
    if(i > 0 && regress[i - 1])
    {
      curr = regMat(regRow, _);
      regRow++;
    }

    for(int j = 0; j < nBoth; j++)
    {
      double tm = mat(i, j);
      if(tm > 0)
      {
        curr[tm - 1] = mat(i, nBoth + 4 + j);
      }
    }

    if(group[i])
    {
      out(outRow, _) = curr;
      outRow++;
    }
  }
  return out;
}

