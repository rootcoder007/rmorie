% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_forwsr_consistency_factor}
\alias{morie_forwsr_consistency_factor}
\title{Consistency factor for a scale estimated from a truncated sample}
\usage{
morie_forwsr_consistency_factor(m, n)
}
\arguments{
\item{m}{Subset size.}
\item{n}{Sample size.}
}
\description{
The subset of size \code{m} holds the \code{m} SMALLEST squared residuals, so \code{s^2} estimates a truncated normal variance and is biased low; on clean data the raw scale climbs sixteenfold across the search as that bias unwinds. Riani, Atkinson and Cerioli\'s equation (12) gives the inflation factor \code{c_FS(m) = (m/n) / P(chi2_(v+2) < chi2_(v,m/n))}; for regression residuals \code{v = 1}, and with \code{psi = Phi^-1{(n+m)/2n}} the denominator is \code{m/n - 2 psi phi(psi)}, so the reciprocal is exactly \code{1 - (2n/m) psi phi(psi)}. Multiply \code{s^2} by it.
}
\value{
The multiplier, 1 when \code{m >= n}.
}
\keyword{internal}
\examples{
morie_forwsr_consistency_factor(m = 5L, n = 5L)
}
