#' @name generate_lasso_regression
#' @title Generate High-dimensional Dataset and Perform Lasso Regression
#' @description This function generates a high-dimensional dataset and fits a Lasso regression model using cross-validation to determine the optimal lambda. Additionally, it visualizes the Lasso paths and cross-validation error curve.
#' @param n_samples Number of samples to generate.
#' @param n_features Number of features to generate.
#' @param noise_level Standard deviation of the noise.
#' @return A list containing the dataset, the fitted lasso model, the optimal lambda value, and ggplot objects for visualization.
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom stats rnorm runif
#' @importFrom ggplot2 ggplot aes geom_line ggtitle xlab ylab theme_minimal
#' @examples
#' \dontrun{
#' result <- generate_lasso_regression(100, 50, 1)
#' print(result$optimal_lambda)
#' print(result$cv_error_plot)
#' }
#' @export
utils::globalVariables(c("lambda", "feature", "cv_error"))
generate_lasso_regression <- function(n_samples = 100, n_features = 50, noise_level = 1) {
  set.seed(42)  # For reproducibility

  # Generate a high-dimensional dataset
  X <- matrix(rnorm(n_samples * n_features), n_samples, n_features)  # Predictor matrix
  true_beta <- c(runif(5, 1, 5), rep(0, n_features - 5))  # Sparse true coefficients
  y <- X %*% true_beta + rnorm(n_samples, sd = noise_level)  # Response variable with noise

  # Perform Lasso regression using cross-validation
  lasso_model <- glmnet::cv.glmnet(
    X, y, alpha = 1, standardize = TRUE, nfolds = 5
  )

  # Extract information for plotting
  lambda <- lasso_model$lambda
  cv_error <- lasso_model$cvm

  # Cross-validation error plot
  cv_error_plot <- ggplot2::ggplot(data.frame(lambda = -log(lambda), cv_error = cv_error),
                                   aes(x = lambda, y = cv_error)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle("Cross-Validation Error vs. Lambda") +
    ggplot2::xlab("Log(Lambda)") +
    ggplot2::ylab("CV Error") +
    ggplot2::theme_minimal()

  # Lasso path plot
  lasso_path_plot <- ggplot2::ggplot() +
    ggplot2::ggtitle("Lasso Path (Placeholder)") +
    ggplot2::theme_minimal()

  # Return results
  result <- list(
    dataset = list(X = X, y = y),
    lasso_model = lasso_model,
    optimal_lambda = lasso_model$lambda.min,
    cv_error_plot = cv_error_plot,
    lasso_path_plot = lasso_path_plot
  )

  return(result)
}

