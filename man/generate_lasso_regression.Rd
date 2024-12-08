% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_lasso_regression.R
\name{generate_lasso_regression}
\alias{generate_lasso_regression}
\title{Generate High-dimensional Dataset and Perform Lasso Regression}
\arguments{
\item{n_samples}{Number of samples to generate.}

\item{n_features}{Number of features to generate.}

\item{noise_level}{Standard deviation of the noise.}
}
\value{
A list containing the dataset, the fitted lasso model, the optimal lambda value, and ggplot objects for visualization.
}
\description{
This function generates a high-dimensional dataset and fits a Lasso regression model using cross-validation to determine the optimal lambda. Additionally, it visualizes the Lasso paths and cross-validation error curve.
}
\examples{
\dontrun{
result <- generate_lasso_regression(100, 50, 1)
print(result$optimal_lambda)
print(result$cv_error_plot)
}
}
