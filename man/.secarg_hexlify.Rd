% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.secarg_hexlify}
\alias{.secarg_hexlify}
\title{.secarg_hexlify}
\usage{
.secarg_hexlify(bs)
}
\arguments{
\item{bs}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the secarg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.secarg_hexlify(V)
}
