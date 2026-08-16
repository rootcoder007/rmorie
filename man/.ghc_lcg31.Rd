% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_lcg31}
\alias{.ghc_lcg31}
\title{Glibc\\'s LCG, exactly. Several modules mirror a Python arm that does}
\usage{
.ghc_lcg31(st)
}
\arguments{
\item{st}{Numeric; combined arithmetically in the body.}
}
\description{
(1103515245 * st + 12345) \% (1 << 31) in big integers. Written directly in R that product reaches 2.4e18, far past the 2^53 where a double stops being exact -- and with integer literals it overflows to NA outright. Split the state into 16-bit limbs and reduce each partial product before recombining, so every intermediate stays below 2^53.
}
\value{
A numeric value.
}
\keyword{internal}
