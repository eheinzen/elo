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
                     NumericVector initialElo, int flag)
{
  int nr = winsA.size() + 1, nc = initialElo.size();
  NumericMatrix out(nr, nc);
  double tmp = 0;
  double e1, e2, j1, j2;

  out(0, _) = initialElo;

  for(int i=1; i < nr; i++)
  {
    out(i, _) = out(i-1, _);

    if(flag == 1)
    {
      e1 = teamA[i-1];
    } else
    {
      j1 = teamA[i-1];
      e1 = out(i, j1);
    }

    if(flag == 2)
    {
      e2 = teamB[i-1];
    } else
    {
      j2 = teamB[i-1];
      e2 = out(i, j2);
    }

    tmp = eloUpdate(e1 + adjTeamA[i-1], e2 + adjTeamB[i-1], winsA[i-1], k[i-1]);

    if(flag != 1)
    {
      out(i, j1) = out(i, j1) + tmp;
    }
    if(flag != 2)
    {
      out(i, j2) = out(i, j2) - tmp;
    }

  }

  return out;
}

