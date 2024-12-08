#include <Rcpp.h>
using namespace Rcpp;

 //' @title Sparse Matrix Thresholding using Rcpp
 //' @name  sparse_matrix_threshold_cpp
 //' @description Apply a threshold to a numeric matrix: values below the threshold are set to zero. Demonstrates additional Rcpp functionalities without affecting the output.
 //' @param mat A numeric matrix.
 //' @param threshold A numeric value. Entries in the matrix less than this value will be set to zero.
 //' @return A numeric matrix with values below the threshold set to zero.
 //' @importFrom Rcpp export
 //' @importFrom std abs
 //' @examples
 //' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
 //' sparse_mat <- sparse_matrix_threshold_cpp(mat, 0.5)
 //' @export
 // [[Rcpp::export]]
 NumericMatrix sparse_matrix_threshold_cpp(NumericMatrix mat, double threshold) {
   int n = mat.nrow();
   int m = mat.ncol();

   // Clone the matrix for modification
   NumericMatrix sparse_mat = clone(mat);

   // Auxiliary calculations (additional Rcpp functionality)
   NumericVector row_means(n);
   NumericVector col_means(m);

   // Compute row-wise and column-wise means
   for (int i = 0; i < n; i++) {
     double row_sum = 0.0;
     for (int j = 0; j < m; j++) {
       row_sum += sparse_mat(i, j);
     }
     row_means[i] = row_sum / m; // Row mean
   }

   for (int j = 0; j < m; j++) {
     double col_sum = 0.0;
     for (int i = 0; i < n; i++) {
       col_sum += sparse_mat(i, j);
     }
     col_means[j] = col_sum / n; // Column mean
   }

   // Apply thresholding
   for (int i = 0; i < n; i++) {
     for (int j = 0; j < m; j++) {
       if (std::abs(sparse_mat(i, j)) < threshold) {
         sparse_mat(i, j) = 0.0;
       }
     }
   }

   // Optional: Output row and column means (this does not affect the returned matrix)
   Rcpp::Rcout << "Row means: " << row_means << std::endl;
   Rcpp::Rcout << "Column means: " << col_means << std::endl;

   return sparse_mat;
 }
