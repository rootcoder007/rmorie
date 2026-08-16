% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_logsumexp}
\alias{.w3_logsumexp}
\title{Log(sum(exp(v))), shifted by the maximum so it cannot overflow}
\usage{
.w3_logsumexp(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{morie_snpest}, \code{morie_varqc1_logpdf}, \code{morie_varqc1_mixture}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
