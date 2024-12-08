#' @title Fibonacci Sequence in C++
#' @description Compute the n-th Fibonacci number using a highly efficient C++ implementation.
#' @param n An integer, the position in the Fibonacci sequence (0-based index).
#' @return The n-th Fibonacci number.
#' @useDynLib SA24204134
#' @examples
#' \dontrun{
#' fibonacci_cpp(10)  # Returns 55
#' }
#' @export
fibonacci_cpp <- function(n) {
  .Call("_SA24204134_fibonacci_cpp", PACKAGE = "SA24204134", n)
}
