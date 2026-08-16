% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fsum}
\alias{.morie_fsum}
\title{Neumaier compensated summation: the R-side counterpart of Python\\'s}
\usage{
.morie_fsum(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
math.fsum (Shewchuk).  On ARM64 R\\'s long double IS double, so base sum() accumulates rounding error this does not; the two language arms then agree to the last bit instead of to the platform.
}
\value{
A numeric value.
}
\keyword{internal}
