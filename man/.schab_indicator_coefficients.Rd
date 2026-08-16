% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_indicator_coefficients}
\alias{.schab_indicator_coefficients}
\title{Exact coefficients of I(Z <= z_k), eq (5.72). Quadrature must NOT be}
\usage{
.schab_indicator_coefficients(z_k, degree)
}
\arguments{
\item{z_k}{See Usage.}
\item{degree}{See Usage.}
}
\description{
used: it is exact for polynomials and the indicator is a step function, so it converges slowly and silently -- and the indicator is the canonical target of the method. b_0 = F(z_k), b_p = (-1)^p H_{p-1}(z_k) f(z_k) / sqrt(p!)
}
\value{
The value of \code{b}, as built in the body.
}
\keyword{internal}
