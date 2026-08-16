% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03maternk}
\alias{.s03maternk}
\title{Equation (2.4.7) p. 18 at a single lag.  Written in logs so that}
\usage{
.s03maternk(d, nu, rho, sigma2)
}
\arguments{
\item{d}{Numeric; combined arithmetically in the body.}
\item{nu}{Numeric; combined arithmetically in the body.}
\item{rho}{Numeric; combined arithmetically in the body.}
\item{sigma2}{Numeric; combined arithmetically in the body.}
}
\description{
large nu does not overflow (z/2)^nu before Gamma(nu) divides it out again.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.s03maternk(d = 5L, nu = 0.5, rho = 0.5, sigma2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
