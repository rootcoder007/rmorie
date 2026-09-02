% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SphVario}
\alias{SphVario}
\title{Gamma(h) = c0 + c{3h/(2a) - (1/2)(h/a)^3} on 0 < h <= a, eq (4.15)}
\usage{
SphVario(h, c0 = 0, c = 1, a = 1) 
}
\arguments{
\item{h}{Passed to \code{.morie_spx_chkv}.}
\item{c0}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{c}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{a}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
plus the nugget of Sec. 4.3.6; covariance eq (4.14).  gamma(0) = 0 ALWAYS -- the discontinuity at the origin IS the nugget, and dropping the h = 0 case is the usual way to lose it.
}
\value{
A list with \code{h}, \code{gamma}, \code{cov}, \code{nugget}, \code{psill}, \code{sill}, \code{range}, \code{true_range}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SphVario(V)
}
