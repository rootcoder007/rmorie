% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ClogSum}
\alias{ClogSum}
\title{Eqs (4.63), (4.65): complex logs of the z-transforms add.  Arg() is a}
\usage{
ClogSum(x, h, z)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
principal value in (-pi, pi], so the two sides can differ by an integer multiple of 2 pi j; that is reported as branch_offset rather than papered over -- it is the phase-unwrapping problem itself.
}
\value{
A list with \code{y}, \code{Y_hat}, \code{X_hat}, \code{H_hat}, \code{magnitude_difference}, \code{branch_offset}, \code{holds_up_to_branch}, \code{method}.
}
\keyword{internal}
\examples{
ClogSum(x = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5, z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
