% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rapaf_levin_ar}
\alias{morie_rapaf_levin_ar}
\title{Levin\'s single-factor attributable risk}
\usage{
morie_rapaf_levin_ar(prevalence, rate_ratio) 
}
\arguments{
\item{prevalence}{Population exposure prevalence, in \code{[0, 1]}.}
\item{rate_ratio}{Rate ratio of exposed to unexposed.}
}
\description{
Computes Levin\'s p(R - 1) / (1 + p(R - 1)) from the population exposure prevalence and the rate ratio.
}
\value{
Numeric attributable risk.
}
\keyword{internal}
\examples{
morie_rapaf_levin_ar(0.2, 2.5)
}
