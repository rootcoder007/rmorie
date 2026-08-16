% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{logmeanexp}
\alias{logmeanexp}
\title{logmeanexp}
\usage{
logmeanexp(values)
}
\arguments{
\item{values}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the pftrep_native implementation. Called by \code{replicated_pfilter}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
logmeanexp(V)
}
