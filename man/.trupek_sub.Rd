% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trupek_sub}
\alias{.trupek_sub}
\title{.trupek_sub}
\usage{
.trupek_sub(g, H, delta, sub, tol, maxit)
}
\arguments{
\item{g}{Passed to \code{.trupek_cauchy}.}
\item{H}{Passed to \code{.trupek_cauchy}.}
\item{delta}{Passed to \code{.trupek_cauchy}.}
\item{sub}{One of \code{"cauchy"}, \code{"dogleg"}, \code{"exact"}.}
\item{tol}{Passed to \code{.trupek_exact}.}
\item{maxit}{Passed to \code{.trupek_exact}.}
}
\description{
A step of the trupek_native implementation. Called by \code{morie_trupek_trust_region}. See the file header for the source the module follows.
}
\value{
The value of \code{.trupek_steihaug}.
}
\references{
  Conn, A.R., Gould, N.I.M. & Toint, P.L. (2000) "Trust-Region
    Methods." MPS-SIAM Series on Optimization, SIAM, Philadelphia.
    Algorithm 6.1.1 (BTR), Section 6.3 (Cauchy point), Section 7.3
    (exact subproblem), Algorithm 7.5.1 (Steihaug-Toint).
  Powell, M.J.D. (1970) "A new algorithm for unconstrained
    optimization", in Nonlinear Programming, Academic Press, 31-65.
  Steihaug, T. (1983) "The conjugate gradient method and trust regions
    in large scale optimization", SIAM Journal on Numerical Analysis
    20(3), 626-637, doi:10.1137/0720042
}
\keyword{internal}
