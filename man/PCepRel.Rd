% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PCepRel}
\alias{PCepRel}
\title{Eq (4.83): y_hat_p(n) = [y_hat(n) + y_hat(-n)]^2 -- the squared even}
\usage{
PCepRel(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
part of the complex cepstrum.  The odd part, where the phase lives, is annihilated by the folding, which is exactly why the power cepstrum loses the phase.
}
\value{
A list with \code{from_complex}, \code{direct}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{phase_lost}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PCepRel(V)
}
