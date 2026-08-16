% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_trupek_trust_region}
\alias{morie_trupek_trust_region}
\title{Trust-region minimisation}
\usage{
morie_trupek_trust_region(f, grad_f, hess_f, x0, delta = 1, delta_max = NULL, subproblem = "steihaug", eta1 = 0.01, eta2 = 0.9, gamma1 = 0.5, gamma3 = 2, max_iter = 200L, gtol = 1e-10, dtol = 1e-14, sub_tol = 1e-12, sub_maxit = 0L)
}
\arguments{
\item{f}{objective, taking a numeric vector.}
\item{grad_f}{gradient, taking a numeric vector.}
\item{hess_f}{Hessian, taking a numeric vector and returning a matrix.}
\item{x0}{starting point.}
\item{delta}{initial radius.}
\item{delta_max}{cap on the radius; defaults to 1e3 times delta.}
\item{subproblem}{steihaug, cauchy, dogleg or exact.}
\item{eta1}{accept the step when the ratio reaches this.}
\item{eta2}{enlarge the radius only when the ratio reaches this.}
\item{gamma1}{shrink factor.}
\item{gamma3}{enlarge factor.}
\item{max_iter}{iteration cap.}
\item{gtol}{stop when the gradient norm falls below this.}
\item{dtol}{stop when the radius collapses below this. Near the   solution the predicted reduction drops under the rounding noise in   f, the ratio stops meaning anything and every step gets rejected;   without this the loop shrinks to zero and burns max_iter.}
\item{sub_tol}{subproblem tolerance.}
\item{sub_maxit}{subproblem iteration cap, 0 for the default.}
}
\description{
Trust-region minimisation
}
\value{
a list with x, fval, gnorm, delta, iterations, accepted,   rejected, converged, exit_reason, history, subproblem,   subproblem_exit and method.
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
