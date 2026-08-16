% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_logistic_propensity}
\alias{.morie_logistic_propensity}
\title{Internal helper: Morie Logistic Propensity}
\usage{
.morie_logistic_propensity(D, X)
}
\description{
Internal helper: Morie Logistic Propensity
}
\value{
The value of \code{pmax}.
}
\keyword{internal}
\examples{
set.seed(2026)
n <- 200L
df <- data.frame(
  D   = rbinom(n, 1, 0.4),
  age = rnorm(n, 50, 10),
  bmi = rnorm(n, 27, 4)
)
df$age[df$D == 1] <- df$age[df$D == 1] + 3 # deliberate imbalance
mrm_standardised_difference(df,
  treatment_col = "D",
  covariates = c("age", "bmi")
)
.morie_logistic_propensity(D = df, X = df)
}
