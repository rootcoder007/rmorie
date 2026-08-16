% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlhoest_rate_requirement}
\alias{morie_tlhoest_rate_requirement}
\title{morie_tlhoest_rate_requirement}
\usage{
morie_tlhoest_rate_requirement(order, n = 1000L)
}
\arguments{
\item{order}{See Usage.}
\item{n}{Defaults to \code{1000L}.}
}
\description{
Part of the tlhoest_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{order}, \code{required_rate_per_nuisance}, \code{example_n}, \code{error_at_that_rate}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 26 (Carone,
Diaz & van der Laan): generalising the TMLE framework to use
higher-order rather than first-order asymptotic representations, to
provide guidelines for estimators with sound finite-sample behaviour
that are asymptotically efficient under less restrictive conditions;
the requirement that the second-order remainder tend to zero faster
than n^{-1/2} for the first-order representation to be useful; and the
example that when the density of the data-generating distribution is
directly involved in the target parameter, a density estimator
converging faster than n^{-1/4} in a suitable norm is often required
to make that remainder negligible.

Carone, M., Diaz, I. & van der Laan, M. J. (2018) "Higher-Order
Targeted Loss-Based Estimation", in *Targeted Learning in Data
Science*, Springer, 483-510, doi:10.1007/978-3-319-65304-4_26.

Robins, J., Li, L., Tchetgen Tchetgen, E. & van der Vaart, A. (2008)
"Higher order influence functions and minimax estimation of nonlinear
functionals", in *Probability and Statistics: Essays in Honor of
David A. Freedman*, IMS, 335-421, doi:10.1214/193940307000000527.
Higher-order influence functions.
}
\keyword{internal}
\examples{
morie_tlhoest_rate_requirement(order = 5L)
}
