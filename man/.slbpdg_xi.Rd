% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.slbpdg_xi}
\alias{.slbpdg_xi}
\title{The deterministic bound xi_k = (1 - kappa) kappa^(k-1), built by}
\usage{
.slbpdg_xi(kappa, k)
}
\arguments{
\item{kappa}{Numeric; combined arithmetically in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
repeated multiplication rather than kappa^(k-1): R\\'s `^` on an integer exponent is repeated squaring and Python\\'s `**` calls pow(), and the two part company in the last bit exactly where the comparison xi_k > u_i decides how many components to carry.
}
\value{
A numeric value.
}
\references{
  Walker, S.G. (2007) "Sampling the Dirichlet mixture model with
    slices." Communications in Statistics -- Simulation and
    Computation 36(1), 45-54.
  Kalli, M., Griffin, J.E. and Walker, S.G. (2011) "Slice sampling
    mixture models." Statistics and Computing 21(1), 93-105.
  Escobar, M.D. and West, M. (1995) "Bayesian density estimation and
    inference using mixtures." JASA 90(430), 577-588, section 6.
  Ishwaran, H. and James, L.F. (2001) "Gibbs sampling methods for
    stick-breaking priors." JASA 96(453), 161-173.
}
\keyword{internal}
