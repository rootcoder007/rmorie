% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sdpwts_is_psd}
\alias{sdpwts_is_psd}
\title{Eigenvalue test on the constraint matrix}
\usage{
sdpwts_is_psd(M, tol = -1e-10)
}
\arguments{
\item{M}{See Usage.}
\item{tol}{Defaults to \code{-1e-10}.}
}
\description{
Part of the sdpwts_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{eigenvalues}, \code{min_eigenvalue}, \code{psd}, \code{strictly_feasible}.
}
\references{
----------
Boyd, S. & Vandenberghe, L. (2004) *Convex Optimization*, Cambridge
University Press, doi:10.1017/CBO9780511804441. Sec. 4.6.2 (the
semidefinite program with its linear matrix inequality constraint,
and LP as the case of diagonal matrices); Sec. 9.6 and 11.1 (the
logarithmic barrier -log det X for the PSD cone, its self-concordance,
and the central path); Sec. 11.2-11.3 (the barrier method: solve the
centring problem for a sequence of increasing t, with the duality gap
at a central point equal to m/t, and the trade-off in the choice of
the multiplier mu between the number of outer iterations and the
difficulty of each centring step); and Sec. 5.5 (the KKT conditions
and complementary slackness used for the optimality check).

Vandenberghe, L. & Boyd, S. (1996) "Semidefinite Programming",
*SIAM Review* 38(1), 49-95, doi:10.1137/1038003. The survey
treatment, including the eigenvalue problems that reduce to SDP.

Nesterov, Y. & Nemirovskii, A. (1994) *Interior-Point Polynomial
Algorithms in Convex Programming*, SIAM,
doi:10.1137/1.9781611970791. Self-concordance, which is what makes
the barrier method\'s complexity claim hold.
}
\keyword{internal}
