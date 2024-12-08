#' @title Sparse Matrix Thresholding using Rcpp
#' @description Apply a threshold to a numeric matrix: values below the threshold are set to zero.
#' @param mat A numeric matrix.
#' @param threshold A numeric value. Entries in the matrix less than this value will be set to zero.
#' @return A numeric matrix with values below the threshold set to zero.
#' @useDynLib SA24204134
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' sparse_mat <- sparse_matrix_threshold_cpp(mat, 0.5)
#' @export
sparse_matrix_threshold_cpp <- function(mat, threshold) {
  .Call('_SA24204134_sparse_matrix_threshold_cpp', mat, threshold, PACKAGE = 'SA24204134')
}
