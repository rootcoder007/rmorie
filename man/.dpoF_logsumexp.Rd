% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpoF_logsumexp}
\alias{.dpoF_logsumexp}
\title{.dpoF_logsumexp}
\usage{
.dpoF_logsumexp(vals)
}
\arguments{
\item{vals}{A vector; its length is taken.}
}
\description{
A step of the dpoF_native implementation. Called by \code{.dpoF_plackett_luce}, \code{optimal_policy}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dpoF_logsumexp(V)
}
