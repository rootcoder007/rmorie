% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{g_computation}
\alias{g_computation}
\title{g_computation}
\usage{
g_computation(strata, outcome_means, covariate_probs) 
}
\arguments{
\item{strata}{Coerced to list by the body, with \code{as.list}.}
\item{outcome_means}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{covariate_probs}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlgcmp_native implementation. Called by \code{morie_tlgcmp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
g_computation(list("a", "b"), outcome_means = c(0.3, 0.7),
              covariate_probs = c(0.4, 0.6))
}
