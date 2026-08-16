% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MinPhase}
\alias{MinPhase}
\title{Section 4.7.2, after eq (4.73): a minimum-phase signal has a CAUSAL}
\usage{
MinPhase(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
complex cepstrum.  Folding the anticausal half onto the causal half reflects every root outside the unit circle to its reciprocal inside, leaving the magnitude spectrum untouched -- which is what "correspondent" means, and is the check returned.
}
\value{
A list with \code{y}, \code{cepstrum}, \code{n}, \code{magnitude_error}, \code{magnitude_preserved}, \code{energy_front_loaded}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MinPhase(V)
}
