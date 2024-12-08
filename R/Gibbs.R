#' @title A Gibbs sampler using Rcpp
#' @name gibbsC
#' @description This function performs Gibbs sampling using Rcpp for efficient computation.
#' @param N Integer, the number of samples to generate.
#' @param thin Integer, the number of iterations between each sample.
#' @return A matrix with N rows and 2 columns. Each row contains a sample of the two variables.
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib SA24204134
#' @examples
#' \dontrun{
#' # Perform Gibbs sampling
#' samples <- gibbsC(100, 10)
#'
#' # Inspect the first few samples
#' print(head(samples))
#'
#' # Plot the sampled variables
#' par(mfrow = c(2, 1))
#' plot(samples[, 1], type = "l", main = "Sampled X", xlab = "Iteration", ylab = "X")
#' plot(samples[, 2], type = "l", main = "Sampled Y", xlab = "Iteration", ylab = "Y")
#' }
#' @export
gibbsC <- function(N, thin) {
  .Call("_SA24204134_gibbsC", N, thin)
}
