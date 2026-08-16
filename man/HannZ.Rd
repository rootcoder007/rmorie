% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannZ}
\alias{HannZ}
\title{Eq (3.102): convolution in time is multiplication in z, so the whole}
\usage{
HannZ(X, z)
}
\arguments{
\item{X}{Coerced to complex by the body, with \code{as.complex}.}
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
filter is one factor multiplying the input transform.  Dividing out X(z) gives eq (3.103) -- which is why the transfer function does not depend on the input.
}
\value{
A list with \code{Y}, \code{H}, \code{X}, \code{z}, \code{transfer_function_is_input_independent}, \code{method}.
}
\keyword{internal}
\examples{
HannZ(X = c(1, 2, 3, 4, 5, 6, 7, 8), z = 5L)
}
