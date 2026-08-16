% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fit_propensity}
\alias{.morie_fit_propensity}
\title{Internal helper: Morie Fit Propensity}
\usage{
.morie_fit_propensity(data, treatment, covariates)
}
\description{
Internal helper: Morie Fit Propensity
}
\value{
The value of \code{pmin}.
}
\keyword{internal}
\examples{
set.seed(1)
df <- data.frame(
  y = rbinom(200, 1, 0.4),
  x1 = rnorm(200),
  x2 = rnorm(200),
  w = runif(200, 0.5, 1.5)
)
morie_run_weighted_logistic_analysis(df,
  outcome = "y", predictors = c("x1", "x2"), weights_col = "w"
)
.morie_fit_propensity(data = df, treatment = df, covariates = df)
}
