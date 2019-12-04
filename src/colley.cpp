#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List eloColley(NumericMatrix teamA, NumericMatrix teamB, NumericVector winsA, NumericVector weightsA, NumericVector weightsB,
               NumericVector weights, NumericMatrix k, int nTeams)
{
  // this function uses 0-based indexing, since the incoming vectors used -1L
  int nGames = winsA.size();
  int ncolA = teamA.ncol(), ncolB = teamB.ncol();

  NumericMatrix out(nTeams, nTeams);
  NumericVector B(nTeams);

  for(int t = 0; t < nTeams; t++)
  {
    out(t, t) = 2.0;
    B(t) = 1.0;
  }


  for(int g = 0; g < nGames; g++)
  {
    for(int i = 0; i < ncolA; i++)
    {
      for(int j = 0; j < ncolB; j++)
      {
        int a = teamA(g, i), b = teamB(g, j);
        double w = weights[g]*weightsA[i]*weightsB[j];
        double iWon = (winsA[g] - 0.5);

        out(b, a) -= w;
        out(a, b) -= w;

        out(a, a) += w;
        out(b, b) += w;

        B[a] += w*iWon*k(g, 0);
        B[b] -= w*iWon*k(g, 1);
      }
    }
  }

  return List::create(out, B);
}

