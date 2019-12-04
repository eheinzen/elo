#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List eloMarkovChain(NumericMatrix teamA, NumericMatrix teamB, NumericVector winsA, NumericVector weightsA, NumericVector weightsB,
                    NumericVector weights, NumericMatrix k, int nTeams)
{
  // this function uses 0-based indexing, since the incoming vectors used -1L
  int nGames = winsA.size();
  int ncolA = teamA.ncol(), ncolB = teamB.ncol();

  NumericMatrix out(nTeams, nTeams);
  NumericVector N_i(nTeams);

  for(int g = 0; g < nGames; g++)
  {
    for(int i = 0; i < ncolA; i++)
    {
      for(int j = 0; j < ncolB; j++)
      {
        int a = teamA(g, i), b = teamB(g, j);
        double w = weights[g]*weightsA[i]*weightsB[j];
        N_i[a] += w;
        N_i[b] += w;
        double iWon = winsA[g];
        // (to, from)
        out(b, a) += w*(k(g, 0)*(1.0 - iWon) + (1.0 - k(g, 0))*iWon); // if j won, go to j with prob=k; else if i won, go with prob=(1-k)
        out(a, b) += w*(k(g, 1)*iWon + (1.0 - k(g, 1))*(1.0 - iWon));

        out(a, a) += w*(k(g, 0)*iWon + (1.0 - k(g, 0))*(1.0 - iWon));
        out(b, b) += w*(k(g, 1)*(1.0 - iWon) + (1.0 - k(g, 1))*iWon);
      }
    }
  }

  for(int j = 0; j < nTeams; j++)
  {
    if(N_i[j] > 0.0)
    {
      out(_, j) = out(_, j) / N_i[j];
    }
  }

  return List::create(out, N_i);
}

