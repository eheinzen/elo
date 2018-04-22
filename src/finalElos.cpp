#include <Rcpp.h>
using namespace Rcpp;

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
  int nBoth = (mat.ncol() - 3) / 2;
  NumericVector out(nTeams);
  int t1 = 0, t2 = 0;
  for(int row = mat.nrow() - 1; row > -1; row--)
  {
    for(int j = 0; j < nBoth; j++)
    {
      double tm = mat(row, j) - 1;
      if(tm >= 0 && out[tm] == 0)
      {
        out[tm] = mat(row, nBoth + 3 + j);
      }
    }

    if(!anyZero(out))
    {
      return out;
    }
  }
  return out;
}



