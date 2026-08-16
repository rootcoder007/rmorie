% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tcmech_floor}
\alias{morie_tcmech_floor}
\title{The epsilon a truncated guarantee cannot get below, at any rho}
\usage{
morie_tcmech_floor(delta, omega = NULL)
}
\arguments{
\item{delta}{The failure probability.}
\item{omega}{The truncation order, or NULL.}
}
\description{
On the fixed-order branch the bound is rho omega + log(1/delta) / (omega - 1), and the second term does not depend on rho at all. So a tight truncation puts a FLOOR under epsilon that no amount of noise removes -- spending less budget shrinks the first term towards zero and leaves the second exactly where it was. This is a real property of truncating the divergence, not a defect of the implementation, and it is the thing that has to be checked before inverting.
}
\value{
The floor, zero when untruncated.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_tcmech_floor(V)
}
