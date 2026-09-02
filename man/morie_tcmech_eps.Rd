% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tcmech_eps}
\alias{morie_tcmech_eps}
\title{The (epsilon, delta) guarantee a rho-tCDP mechanism gives}
\usage{
morie_tcmech_eps(rho, delta, omega = NULL) 
}
\arguments{
\item{rho}{The concentrated-privacy budget.}
\item{delta}{The failure probability.}
\item{omega}{The truncation order, or NULL for untruncated zCDP.}
}
\description{
With no truncation this is Bun and Steinke's Proposition 1.3, exactly. With a finite omega the optimising order may lie outside the range where the divergence is bounded, and the fixed-order Renyi conversion is used instead -- which is strictly worse, as it must be, because a weaker premise cannot give a stronger promise.
}
\value{
The epsilon achieved.
}
\references{
  Bun, M., Dwork, C., Rothblum, G.N. and Steinke, T. (2018)
    "Composable and versatile privacy via truncated CDP." Proceedings
    of the 50th Annual ACM SIGACT Symposium on Theory of Computing
    (STOC), 74-86. doi:10.1145/3188745.3188946.
  Bun, M. and Steinke, T. (2016) "Concentrated differential privacy:
    simplifications, extensions, and lower bounds." Theory of
    Cryptography Conference (TCC), 635-658. arXiv:1605.02065.
    Definition 1.1, Proposition 1.3, Proposition 1.4, Definition 1.5
    and Proposition 1.6.
  Mironov, I. (2017) "Renyi differential privacy." IEEE Computer
    Security Foundations Symposium (CSF), 263-275.
  Dwork, C. and Rothblum, G.N. (2016) "Concentrated differential
    privacy." arXiv:1603.01887.
}
\keyword{internal}
\examples{
morie_tcmech_eps(rho = 0.5, delta = 0.5)
}
