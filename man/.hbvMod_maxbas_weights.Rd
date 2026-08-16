% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hbvMod_maxbas_weights}
\alias{.hbvMod_maxbas_weights}
\title{Eq. 6 of Seibert & Vis (2012): triangular routing weights}
\usage{
.hbvMod_maxbas_weights(maxbas)
}
\arguments{
\item{maxbas}{See Usage.}
}
\description{
c(i) = int_{i-1}^{i} of (2/M - |u - M/2| * 4/M^2) du
}
\value{
The value of \code{w}, as built in the body.
}
\keyword{internal}
