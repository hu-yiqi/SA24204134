#include <Rcpp.h>
using namespace Rcpp;

 //' @title Compute Fibonacci Sequence
 //' @name fibonacci_cpp
 //' @description Efficiently compute the n-th Fibonacci number using C++ and Rcpp utilities.
 //' @param n An integer, the position in the Fibonacci sequence (0-based index).
 //' @return The n-th Fibonacci number.
 //' @importFrom Rcpp export
 //' @examples
 //' \dontrun{
 //' fibonacci_cpp(10)  # Returns 55
 //' }
 //' @export
 // [[Rcpp::export]]
 int fibonacci_cpp(int n) {
   // Validate input
   if (n < 0) stop("n must be a non-negative integer.");

   // Clone input (not strictly necessary here, but demonstrates Rcpp functionality)
   IntegerVector n_vec = wrap(n); // Wrap the input into an Rcpp object
   IntegerVector n_clone = clone(n_vec); // Clone the wrapped object
   int n_cloned_value = n_clone[0]; // Extract the integer value

   // Prepare Fibonacci computation
   if (n_cloned_value == 0) return 0;
   if (n_cloned_value == 1) return 1;

   int prev = 0, curr = 1, next = 0;

   // Use Rcpp to log progress
   Rcout << "Computing Fibonacci sequence up to position " << n_cloned_value << "...\n";

   for (int i = 2; i <= n_cloned_value; ++i) {
     next = prev + curr;
     prev = curr;
     curr = next;

     // Print the current Fibonacci number
     Rcout << "Step " << i << ": " << curr << "\n";
   }

   return curr;
 }
