#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List eloWinPct(NumericVector winsA, NumericMatrix teamA, NumericMatrix teamB,
               NumericVector weightsA, NumericVector weightsB, NumericVector weights, int nTeams) {
  NumericVector out(nTeams), N_i(nTeams);
  int ncolA = teamA.ncol(), ncolB = teamB.ncol();

  for(int i = 0; i < winsA.size(); i++)
  {
    for(int j = 0; j < ncolA; j++)
    {
      double tmA = teamA(i, j);
      out[tmA] += weights[i]*weightsA[j]*winsA[i];
      N_i[tmA] += weights[i]*weightsA[j];
    }

    for(int j = 0; j < ncolB; j++)
    {
      double tmB = teamB(i, j);
      out[tmB] += weights[i]*weightsB[j]*(1.0 - winsA[i]);
      N_i[tmB] += weights[i]*weightsB[j];
    }
  }

  return List::create(out / N_i, N_i);
}

