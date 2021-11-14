#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericMatrix legendre(Rcpp::NumericVector x, int p) {
  int n = x.size();
  Rcpp::IntegerVector polies (p+1);
  for(int i=0; i < polies.length(); i++)
    polies[i] = (i*2+1);
  Rcpp::NumericVector squirts = sqrt(polies);
  Rcpp::NumericMatrix y(n, p);
  for(int i = 0; i < n; ++i) {
    y(i,0) = 2 * x[i] - 1;
    y(i,1) = (3 * y(i,0) * y(i,0) - 1)/2;
  }
  for(int j=2; j < y.ncol(); j++){
    for(int i = 0; i < n; ++i) {
      y(i,j) = (polies[j] * y(i,0) * y(i,j-1) - j * y(i,j-2))/(j+1);
    }
  }
  for(int j=0; j < y.ncol(); j++){
    for(int i = 0; i < n; ++i)
      y(i,j) = squirts[j+1] * y(i,j);
  }
  return y;
}
