#include <Rcpp.h>
using namespace Rcpp;

 //' @title A Gibbs sampler using Rcpp
 //' @name gibbsC
 //' @description A Gibbs sampler implemented in Rcpp for efficient sampling.
 //' @param N Integer, the number of samples.
 //' @param thin Integer, the number of iterations between samples.
 //' @return A NumericMatrix with \code{N} rows and 2 columns, representing the samples.
 //' @examples
 //' \dontrun{
 //' rnC <- gibbsC(100, 10)
 //' print(head(rnC))
 //' }
 //' @export
 // [[Rcpp::export]]
 NumericMatrix gibbsC(int N, int thin) {
   NumericMatrix samples(N, 2); // Initialize matrix for storing samples
   double x = 0.0, y = 0.0; // Initialize variables

   for (int i = 0; i < N; ++i) {
     for (int j = 0; j < thin; ++j) {
       x = R::rgamma(3, 1 / (y * y + 4)); // Sample x from Gamma distribution
       y = R::rnorm(1 / (x + 1), 1 / sqrt(2 * (x + 1))); // Sample y from Normal distribution
     }
     samples(i, 0) = x;
     samples(i, 1) = y;
   }

   return samples;
 }
