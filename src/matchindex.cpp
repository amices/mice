#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace std;
using namespace Rcpp;

//' Find index of matched donor units
//'
//' @param d   Numeric vector with values from donor cases.
//' @param t   Numeric vector with values from target cases.
//' @param k   Integer, number of unique donors from which a random draw is made.
//' For \code{k = 1} the function returns the index in \code{d} corresponding
//' to the closest unit. For multiple imputation, the
//' advice is to set values in the range of \code{k = 5} to \code{k = 10}.
//' @return An integer vector with \code{length(t)} elements. Each
//' element is an index in the array \code{d}.
//' @details
//' For each element in \code{t}, the method finds the \code{k} nearest
//' neighbours in \code{d}, randomly draws one of these neighbours, and
//' returns its position in vector \code{d}.
//'
//' Fast predictive mean matching algorithm in seven steps:
//'
//' 1. Shuffle records to remove effects of ties
//'
//' 2. Obtain sorting order on shuffled data
//'
//' 3. Calculate index on input data and sort it
//'
//' 4. Pre-sample vector \code{h} with values between 1 and \code{k}
//'
//' For each of the \code{n0} elements in \code{t}:
//'
//'   5. find the two adjacent neighbours
//'
//'   6. find the \code{h_i}'th nearest neighbour
//'
//'   7. store the index of that neighbour
//'
//' Return vector of \code{n0} positions in \code{d}.
//'
//' We may use the function to perform predictive mean matching under a given
//' predictive model. To do so, specify both \code{d} and \code{t} as
//' predictions from the same model. Suppose that \code{y} contains the observed
//' outcomes of the donor cases (in the same sequence as \code{d}), then
//' \code{y[matchindex(d, t)]} returns one matched outcome for every
//' target case.
//'
//' See \url{https://github.com/amices/mice/issues/236}.
//' This function is a replacement for the \code{matcher()} function that has
//' been in default in \code{mice} since version \code{2.22} (June 2014).
//' @examples
//' set.seed(1)
//'
//' # Inputs need not be sorted
//' d <- c(-5, 5, 0, 10, 12)
//' t <- c(-6, -4, 0, 2, 4, -2, 6)
//'
//' # Index (in vector a) of closest match
//' idx <- matchindex(d, t, 1)
//' idx
//'
//' # To check: show values of closest match
//'
//' # Random draw among indices of the 5 closest predictors
//' matchindex(d, t)
//'
//' # An example
//' train <- mtcars[1:20, ]
//' test <- mtcars[21:32, ]
//' fit <- lm(mpg ~ disp + cyl, data = train)
//' d <- fitted.values(fit)
//' t <- predict(fit, newdata = test)  # note: not using mpg
//' idx <- matchindex(d, t)
//'
//' # Borrow values from train to produce 12 synthetic values for mpg in test.
//' # Synthetic values are plausible values that could have been observed if
//' # they had been measured.
//' train$mpg[idx]
//'
//' # Exercise: Create a distribution of 1000 plausible values for each of the
//' # twelve mpg entries in test, and count how many times the true value
//' # (which we know here) is located within the inter-quartile range of each
//' # distribution. Is your count anywhere close to 500? Why? Why not?
//' @author Stef van Buuren, Nasinski Maciej, Alexander Robitzsch
//' @export
// [[Rcpp::export]]
IntegerVector matchindex(NumericVector d, NumericVector t, int k = 5) {

  Environment base("package:base");
  Function sample = base["sample"];

  // declarations
  int n1 = d.size();
  int n0 = t.size();

  // 1. Shuffle records to remove effects of ties
  // Suggested by Alexander Robitzsch
  // https://github.com/stefvanbuuren/mice/issues/236
  // Call base::sample() to advance .Random.seed
  IntegerVector ishuf= sample(n1);
  ishuf = ishuf - 1;
  NumericVector yshuf(n1);
  for (int i = 0; i < n1; i++) {yshuf(i) = d(ishuf(i));}

  // 2. Obtain sorting order on shuffled data
  // https://stackoverflow.com/questions/1577475/c-sorting-and-keeping-track-of-indexes
  IntegerVector isort(n1);
  iota(isort.begin(), isort.end(), 0);
  stable_sort(isort.begin(), isort.end(),
              [yshuf](int i1, int i2) {return yshuf[i1] < yshuf[i2];});

  // 3. Calculate index on input data and sort
  IntegerVector id(n1);
  std::vector<double> ysort(n1);
  for (int i = 0; i < n1; i++) {
    id(i) = ishuf(isort(i));
    ysort[i] = d(id(i));
  }

  // 4. Pre-sample n0 values between 1 and k
  // restrict 1 <= k <= n1
  k = (k <= n1) ? k : n1;
  k = (k >= 1) ? k : 1;
  IntegerVector kv(k);
  iota(kv.begin(), kv.end(), 1);
  IntegerVector h = sample(kv, n0, Rcpp::_["replace"] = true);

  IntegerVector idx(n0);

  // loop over the target units
  for (int i = 0; i < n0; i++) {
    double val = t(i);
    int hi = h(i);
    int count = 0;

    // 5. find the two adjacent neighbours
    std::vector<double>::iterator iter;
    iter = std::lower_bound(ysort.begin(), ysort.end(), val);
    int r = iter - ysort.begin();
    int l = r - 1;

    // 6. find the h_i'th nearest neighbour
    // 7. store the index of that neighbour

    // Compare elements on left and right of crossover
    // point to find the h'th closest match
    // Inspired on Polkas: https://github.com/Polkas/miceFast/issues/10
    while (count < hi && l >= 0 && r < n1)
    {
      if (val - ysort[l] < ysort[r] - val)
      {
        idx(i) = id[l--];
      } else {
        idx(i) = id[r++];
      }
      count++;
    }

    // If right side is exhausted, take left elements
    while (count < hi && l >= 0)
    {
      idx(i) = id[l--];
      count++;
    }

    // If left side is exhausted, take right elements
    while (count < hi && r < n1)
    {
      idx(i) = id[r++];
      count++;
    }
  }

  return idx + 1;
}
