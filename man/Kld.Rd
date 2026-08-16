% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kld}
\alias{Kld}
\title{Eq (5.33): KLD(p1, p2) = sum_l p2(x_l) ln[p2(x_l) / p1(x_l)]}
\usage{
Kld(p1, p2)
}
\arguments{
\item{p1}{See Usage.}
\item{p2}{See Usage.}
}
\description{
NOTE THE ARGUMENT ORDER -- the book weights by the SECOND PDF, so its KLD(p1, p2) is D_KL(p2 || p1) in standard notation, the REVERSE of what most texts and libraries mean by KL(p, q).  That is the single most likely way to get a wrong number out of this family. KLD is not symmetric, so swapping the arguments gives a different number; both are returned so the asymmetry is visible rather than a trap.  Their sum is exactly the divergence of eq (10.115).
}
\value{
A list with \code{kld}, \code{reversed}, \code{symmetric_sum}, \code{asymmetric}, \code{weighted_by_the_second_pdf}, \code{symmetric_sum_is_the_divergence_of_eq_10_115}, \code{nonnegative}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Kld(V, V)
}
