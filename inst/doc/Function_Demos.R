## -----------------------------------------------------------------------------
# Load the package
library(SA24204134)

n <- 20  # Choose a relatively large value for a meaningful comparison

fibonacci_cpp = fibonacci_cpp(n)
fibonacci_cpp

## -----------------------------------------------------------------------------
# Example usage:
library(SA24204134)

# Generate dataset and fit Lasso model
result <- generate_lasso_regression(n_samples = 100, n_features = 50, noise_level = 1)

# Display optimal lambda
cat("Optimal lambda:", result$optimal_lambda, "\n")

# Plot Cross-validation error
print(result$cv_error_plot)


## -----------------------------------------------------------------------------
library(SA24204134)

# Create a random matrix
mat <- matrix(rnorm(100), nrow = 10, ncol = 10)

# Apply sparse matrix thresholding
sparse_mat <- sparse_matrix_threshold_cpp(mat, 0.5)

# Print the sparse matrix
print(sparse_mat)

# Verify that all values below the threshold are zero
all(abs(sparse_mat[abs(mat) < 0.5]) == 0)

