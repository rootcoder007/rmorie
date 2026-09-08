% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rapaf_population_attributable_risk}
\alias{morie_rapaf_population_attributable_risk}
\title{Population attributable risk}
\usage{
morie_rapaf_population_attributable_risk(case_counts, rate_ratios) 
}
\arguments{
\item{case_counts}{Non-negative numeric vector of case counts per   stratum.}
\item{rate_ratios}{Positive numeric vector of adjusted rate ratios.}
}
\description{
Bruzzi et al. (1985) eq. (6): AR = 1 - sum_j rho_j / R_j, where rho_j is the proportion of cases in stratum j and R_j the adjusted rate ratio of stratum j against the baseline. The control distribution is not used.
}
\value{
A list with \code{estimate}, \code{ar},   \code{case_proportions}, \code{rate_ratios}, \code{n_cases},   \code{n_strata}, \code{uses_control_distribution}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rapaf_population_attributable_risk(V, V)
}
