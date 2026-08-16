% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_bw01}
\alias{.hrz3_bw01}
\title{Default bandwidth on the mid-rank [0, 1] scale.  After .hrz3_u01 the}
\usage{
.hrz3_bw01(n)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
marginals are exactly uniform on [0, 1], whose standard deviation is 1/sqrt(12), so Silverman\\'s constant gives a scale rather than only a rate.  n^(-1/6) alone is a rate and on the unit interval is far too wide: at n = 40 it puts two thirds of the kernel mass outside support.
}
\value{
A numeric value.
}
\keyword{internal}
