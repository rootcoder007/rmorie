% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rr_approx}
\alias{.morie_rr_approx}
\title{Internal helper: approximate risk ratio from another effect scale}
\usage{
.morie_rr_approx(est, type = c("RR", "OR", "HR", "OLS", "MD"), rare = TRUE, sd = NULL)
}
\description{
The published VanderWeele-Ding approximate conversions used by the EValue package: OR/HR (rare vs common outcome) and standardized mean difference / OLS.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
