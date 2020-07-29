#include <Rcpp.h>
#include <algorithm>
using namespace std;
using namespace Rcpp;

int findCrossOver(NumericVector arr, double low, double high, double x) 
{ 
  if (arr[high] <= x) // x is greater than all 
    return high; 
  if (arr[low] > x) // x is smaller than all 
    return low; 
  
  // Find the middle point 
  int mid = (low + high)/2; /* low + (high - low)/2 */
  
  /* If x is same as middle element, then return mid */
  if (arr[mid] <= x && arr[mid+1] > x) 
    return mid; 
  
  /* If x is greater than arr[mid], then either arr[mid + 1] 
   is ceiling of x or ceiling lies in arr[mid+1...high] */
  if(arr[mid] < x) 
    return findCrossOver(arr, mid + 1, high, x); 
  
  return findCrossOver(arr, low, mid - 1, x); 
} 


double Kclosestrand(NumericVector arr, double x, int k) 
{ 
  int n = arr.size();
  // Find the crossover point 
  int l = findCrossOver(arr, 0, n-1, x); 
  int r = l; // Right index to search 
  int count = 0; // To keep track of count of elements already printed 
  NumericVector resus(k);

  // If x is present in arr[], then reduce left index 
  // Assumption: all elements in arr[] are distinct 
  if (arr[l] == x) l--; 
  
  // Compare elements on left and right of crossover 
  // point to find the k closest elements 
  while (l >= 0 && r < n && count < k) 
  { 
    if (x - arr[l] < arr[r] - x) 
      resus[count] = arr[l--]; 
    else
      resus[count] = arr[r++]; 
    count++; 
  } 
  
  // If there are no more elements on right side, then 
  // print left elements 
  while (count < k && l >= 0) 
    resus[count] = arr[l--], count++; 
  
  // If there are no more elements on left side, then 
  // print right elements 
  while (count < k && r < n) 
   resus[count] = arr[r++], count++; 
  
  int goal = rand()%k;
  
  return resus[goal];
 
}


// [[Rcpp::export]]
NumericVector neibo(NumericVector y, NumericVector miss, int k) {
  int n_y = y.size();
  k = (k <= n_y) ? k : n_y;
  k = (k >= 1) ? k : 1;
  
  NumericVector y_new = clone(y);
  
  sort(y_new.begin(),y_new.end());
  
  unsigned int n_miss = miss.size();
  
  NumericVector resus(n_miss);
  
  for(int i=0; i<n_miss ;i++){
    double mm = miss[i];
    resus[i] = Kclosestrand(y_new,mm,k);
  } 
    
  return resus ;
  
}

/* Driver program to check above functions */

/*** R

vals = rnorm(100)

ss = rnorm(100)

neibo(vals,ss,2)[1:10]

vals[mice:::matcher(vals,ss,2)][1:10]

microbenchmark::microbenchmark(neibo(vals,ss,2),
                               
                               mice:::matcher(vals,ss,2)
)

vals = rnorm(10000)

ss = rnorm(1000)

neibo(vals,ss,2)[1:10]

vals[mice:::matcher(vals,ss,2)][1:10]

microbenchmark::microbenchmark(neibo(vals,ss,2),
                               
                               mice:::matcher(vals,ss,2)
)


vals = rnorm(10000)

ss = rnorm(1000)

neibo(vals,ss,200)[1:10]

vals[mice:::matcher(vals,ss,200)][1:10]

microbenchmark::microbenchmark(neibo(vals,ss,200),
                               
                               mice:::matcher(vals,ss,200)
)


vals = 1:10000

ss = 1:100

neibo(vals,ss,2)[1:10]

vals[mice:::matcher(vals,ss,2)][1:10]

microbenchmark::microbenchmark(neibo(vals,ss,2),
                               
                               mice:::matcher(vals,ss,2)
)
*/