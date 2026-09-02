% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tcmech}
\alias{morie_tcmech}
\title{Release a clipped query under a target (epsilon, delta) budget}
\usage{
morie_tcmech(y, f_value, C, epsilon, delta, omega = NULL, seed = 0, n_release = 1L) 
}
\arguments{
\item{y}{The per-record contributions. Clipping them is what gives   the query a sensitivity at all.}
\item{f_value}{The query value being protected.}
\item{C}{The clipping bound, and hence the sensitivity of one record.}
\item{epsilon}{The target epsilon.}
\item{delta}{The target delta.}
\item{omega}{The truncation order, or NULL.}
\item{seed}{The random stream.}
\item{n_release}{How many releases the budget is split across.}
}
\description{
Release a clipped query under a target (epsilon, delta) budget
}
\value{
A list with the private answer, the noise scale, the budget   in rho, how many records the clipping bound, and the guarantee   actually achieved.
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
Y <- vapply(0:29, function(i) round(((i * 13)\%\%41) - 20 + ((i * 
    7)\%\%3) * 0.5, 4), numeric(1))
morie_tcmech(Y, 12.5, 10, 1, 1e-05, NULL, 3)
}
