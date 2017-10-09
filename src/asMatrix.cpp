#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix eloRunAsMatrix(NumericMatrix mat, NumericVector initialElos, LogicalVector group)
{
  // this function uses 1-based indexing, since the incoming matrix does, too
  double nTeams = initialElos.size();
  double nGames = mat.nrow();
  double nOut = sum(group);
  NumericMatrix out(nOut, nTeams);
  NumericVector curr(nTeams);
  curr = initialElos;
  int outRow = 0;

  for(int i = 0; i < nGames; i++)
  {
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

// [[Rcpp::export]]
NumericMatrix eloRunRegressedAsMatrix(NumericMatrix mat, NumericVector initialElos,
                                      NumericMatrix regMat, LogicalVector regress, LogicalVector group)
{
  // this function uses 1-based indexing, since the incoming matrix does, too
  double nTeams = initialElos.size();
  double nGames = mat.nrow();
  double nOut = sum(group);
  NumericMatrix out(nOut, nTeams);
  NumericVector curr(nTeams);
  curr = initialElos;
  int regRow = 0, outRow = 0;

  for(int i = 0; i < nGames; i++)
  {
    if(i > 0 && regress[i - 1])
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

