% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MASS_ginv}
\alias{MASS_ginv}
\title{Moore-Penrose pseudo-inverse, MASS::ginv\'s rule: relative cutoff}
\usage{
MASS_ginv(X, tol = sqrt(.Machine$double.eps)) 
}
\arguments{
\item{X}{A matrix; passed to \code{dim}.}
\item{tol}{Numeric; combined arithmetically in the body.}
}
\description{
tol * d[1] with tol = sqrt(.Machine$double.eps), i.e. ~1.49e-8 of the largest singular value. Kept DISTINCT from .morie_pinv (rcond * max(s), rcond = 1e-15) because they are two different published conventions and code ported from MASS::ginv must keep MASS\'s answer. Mirrors morie.fn._array_core.ginv exactly.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MASS_ginv(V)
}
