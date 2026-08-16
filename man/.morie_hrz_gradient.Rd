% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hrz_gradient}
\alias{.morie_hrz_gradient}
\title{The interior formula must account for UNEQUAL spacing -- the naive}
\usage{
.morie_hrz_gradient(fv, xv)
}
\arguments{
\item{fv}{A vector; its length is taken and its elements indexed.}
\item{xv}{A vector; indexed elementwise.}
}
\description{
(f[i+1] - f[i-1]) / (x[i+1] - x[i-1]) is only second-order accurate on a uniform grid, and the index here is sorted data, which is anything but. Using the naive form put every quantity downstream of G\\' about 1e-4 away from morie.fn, which is small but is not parity.
}
\value{
The value of \code{g}, as built in the body.
}
\keyword{internal}
