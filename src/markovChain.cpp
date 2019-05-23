#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List eloMarkovChain(NumericVector teamA, NumericVector teamB, NumericVector winsA,
                             NumericVector weights, NumericVector k, int nTeams)
{
  // this function uses 0-based indexing, since the incoming vectors used -1L
  int nGames = winsA.size();

  NumericMatrix out(nTeams, nTeams);
  NumericVector N_i(nTeams);

  for(int g = 0; g < nGames; g++)
  {
    int i = teamA[g], j = teamB[g];
    N_i[i] += weights[g];
    N_i[j] += weights[g];
    int iWon = (winsA[g] == 1.0);

    out(j, i) += weights[g]*(k[g]*(1 - iWon) + (1.0 - k[g])*iWon); // if j won, go to j with prob=k; else if i won, go with prob=(1-k)
    out(i, j) += weights[g]*(k[g]*iWon + (1.0 - k[g])*(1 - iWon));

    out(i, i) += weights[g]*(k[g]*iWon + (1.0 - k[g])*(1 - iWon));
    out(j, j) += weights[g]*(k[g]*(1 - iWon) + (1.0 - k[g])*iWon);
  }

  for(int j = 0; j < nTeams; j++)
  {
    out(_, j) = out(_, j) / N_i[j];
  }

  return List::create(out, N_i);
}

