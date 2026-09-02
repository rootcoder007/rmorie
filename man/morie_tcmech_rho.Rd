% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tcmech_rho}
\alias{morie_tcmech_rho}
\title{The largest rho whose guarantee still meets a target (eps, delta)}
\usage{
morie_tcmech_rho(epsilon, delta, omega = NULL, iters = 200L) 
}
\arguments{
\item{epsilon}{The target epsilon.}
\item{delta}{The target delta.}
\item{omega}{The truncation order, or NULL.}
\item{iters}{Bisection steps.}
}
\description{
A fixed-step bisection on a strictly increasing function, so it runs the same number of steps whatever the inputs. The upper bracket is widened by doubling first, which is a fixed schedule and not a search.
}
\value{
The affordable rho.
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
morie_tcmech_rho(epsilon = 5L, delta = 0.5)
}
