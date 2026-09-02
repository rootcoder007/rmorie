% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_local_encoder}
\alias{narm_local_encoder}
\title{narm_local_encoder}
\usage{
narm_local_encoder(H, alpha) 
}
\arguments{
\item{H}{A matrix; passed to \code{as.matrix}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the narm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
narm_local_encoder(V, V)
}
