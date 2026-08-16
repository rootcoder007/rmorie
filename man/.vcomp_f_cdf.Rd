% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vcomp_f_cdf}
\alias{.vcomp_f_cdf}
\title{F CDF via the regularized incomplete beta:}
\usage{
.vcomp_f_cdf(x, d1, d2)
}
\arguments{
\item{x}{See Usage.}
\item{d1}{See Usage.}
\item{d2}{See Usage.}
}
\description{
P(F <= x) = I_{d1 x / (d1 x + d2)}(d1/2, d2/2)
}
\value{
The value of \code{pbeta}.
}
\keyword{internal}
