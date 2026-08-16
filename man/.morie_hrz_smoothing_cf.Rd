% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hrz_smoothing_cf}
\alias{.morie_hrz_smoothing_cf}
\title{Psi_zeta: a bounded real characteristic function supported on}
\usage{
.morie_hrz_smoothing_cf(u)
}
\arguments{
\item{u}{Numeric; passed to \code{abs}.}
}
\description{
[-1, 1] -- the book\\'s example is the fourfold convolution of the uniform density with itself, whose characteristic function is sinc^4. Compact support in tau is the point: it stops the integrand being evaluated where the denominator has died.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_hrz_smoothing_cf(V)
}
