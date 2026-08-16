% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sdpwts_min_eigenvalue_sdp}
\alias{sdpwts_min_eigenvalue_sdp}
\title{Maximise t s.t. A - tI >= 0 (exact answer = lambda_min(A))}
\usage{
sdpwts_min_eigenvalue_sdp(A, t0 = 1, mu = 10, tol = 1e-09)
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{t0}{Defaults to \code{1}.}
\item{mu}{Defaults to \code{10}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
A step of the sdpwts_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{t}, \code{lambda_min}, \code{error}, \code{outer_iterations}, \code{gap}, \code{method}, \code{note}.
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
