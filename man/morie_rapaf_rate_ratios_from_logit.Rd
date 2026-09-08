% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rapaf_rate_ratios_from_logit}
\alias{morie_rapaf_rate_ratios_from_logit}
\title{Rate ratios from a logistic regression}
\usage{
morie_rapaf_rate_ratios_from_logit(case_counts, control_counts, design, ridge = 1e-08) 
}
\arguments{
\item{case_counts}{Non-negative case counts per stratum.}
\item{control_counts}{Non-negative control counts per stratum.}
\item{design}{Numeric matrix of covariates (one row per stratum).}
\item{ridge}{Ridge added to the diagonal of X\'WX.}
}
\description{
Fits a logistic regression to the case / control design matrix and returns the odds ratio of each stratum against the baseline.
}
\value{
A list with \code{rate_ratios}, \code{coef}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rapaf_population_attributable_risk(V, V)
morie_rapaf_rate_ratios_from_logit(case_counts = V, control_counts = V, design = V)
}
