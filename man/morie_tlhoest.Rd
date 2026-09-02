% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlhoest}
\alias{morie_tlhoest}
\title{morie_tlhoest}
\usage{
morie_tlhoest(psi_plugin, D1, D2_kernel, O) 
}
\arguments{
\item{psi_plugin}{Passed to \code{.tlhoest_first_order_expansion}.}
\item{D1}{Passed to \code{.tlhoest_first_order_expansion}.}
\item{D2_kernel}{Passed to \code{.tlhoest_second_order_term}.}
\item{O}{Passed to \code{.tlhoest_second_order_term}.}
}
\description{
A step of the tlhoest_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{first_order}, \code{second_order_correction}, \code{n_pairs}, \code{method}, \code{note}.
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
than n^\{-1/2\} for the first-order representation to be useful; and the
example that when the density of the data-generating distribution is
directly involved in the target parameter, a density estimator
converging faster than n^\{-1/4\} in a suitable norm is often required
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
