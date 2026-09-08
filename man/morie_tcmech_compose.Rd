% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tcmech_compose}
\alias{morie_tcmech_compose}
\title{Composition: the rhos add and the truncation is the tightest one}
\usage{
morie_tcmech_compose(rhos, omegas = NULL) 
}
\arguments{
\item{rhos}{The per-release budgets.}
\item{omegas}{The per-release truncations, or NULL.}
}
\description{
Renyi divergence is additive over independent releases at each order, so the budgets add. A composed mechanism is only bounded at orders where EVERY part is bounded, which is why the truncation takes the minimum -- taking the maximum would claim a guarantee at orders one of the parts never had.
}
\value{
A list with the total rho and the composed truncation.
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
morie_tcmech_compose(V)
}
