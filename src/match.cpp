#include <Rcpp.h>
#include <algorithm>
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector matcher(NumericVector obs, NumericVector mis, int k) {
  // fast predictive mean matching algorithm
  // for each of the n0 elements in mis
  // 1) calculate the difference with obs
  // 2) add small noise to break ties
  // 3) find the k indices of the k closest predictors
  // 4) randomly draw one index
  // and return the vector of n0 matched positions 
  // SvB 26/01/2014

  // declarations
  int j;
  int n1 = obs.size();
  int n0 = mis.size();
  double dk = 0; 
  int count = 0;
  int goal = 0;
  NumericVector d(n1);
  NumericVector d2(n1);
  IntegerVector matched(n0);
  
  // restrict 1 <= k <= n1
  k = (k <= n1) ? k : n1;
  k = (k >= 1) ? k : 1;
  
  // in advance, uniform sample from k potential donors
  NumericVector which = floor(runif(n0, 1, k + 1));
  NumericVector mm = range(obs);
  double jitter = (mm[1] - mm[0]) / 65536;
  
  // loop over the missing values
  for(int i = 0; i < n0; i++) {
      
      // calculate the distance and add noise to break ties
      d = abs(obs - mis[i]);
      d += runif(n1, 0, jitter);
      
      // find the k'th lowest value in d
      for (int j = 0; j < n1; j++) d2[j] = d[j];
      std::nth_element (d2.begin(), d2.begin()+k-1, d2.end());

      // find index of donor which[i]
      dk = d2[k-1];
      count = 0;
      goal = (int) which[i];
      for (j = 0; j < n1; j++) {
          if (d[j] <= dk) count++;
          if (count == goal) break;
      }
      
      // and store the result
      matched[i] = j;
  }
  
  // increase index to offset 1
  return matched + 1;
}
