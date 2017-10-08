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



