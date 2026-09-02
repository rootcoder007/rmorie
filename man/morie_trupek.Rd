% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_trupek}
\alias{morie_trupek}
\title{Trust-region minimisation}
\usage{
morie_trupek(f, grad_f, hess_f, x0, ...) 
}
\arguments{
\item{f}{objective.}
\item{grad_f}{gradient.}
\item{hess_f}{Hessian.}
\item{x0}{starting point.}
\item{...}{passed to morie_trupek_trust_region.}
}
\description{
Trust-region minimisation
}
\value{
see morie_trupek_trust_region.
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
