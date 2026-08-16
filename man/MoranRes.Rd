% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MoranRes}
\alias{MoranRes}
\title{Eq (1.16).  Eg[Ires] = n tr[MW] / {(n-k) w..} is the book\'s own}
\usage{
MoranRes(residuals, w, x = NULL)
}
\arguments{
\item{residuals}{Passed to \code{.morie_spx_chkv}.}
\item{w}{Passed to \code{.morie_spx_chkw}.}
\item{x}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
formula, Sec. 1.3.2, and is reproduced term for term.  The variance is derived from the exact moments of a ratio of quadratic forms in the same Gaussian projection.
}
\value{
A list with \code{i}, \code{expectation}, \code{variance}, \code{z}, \code{p_value}, \code{s0}, \code{tr_mw}, \code{k}, \code{n}, \code{not_minus_one_over_n_minus_one}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
W <- matrix(0, 6, 6)
W[abs(row(W) - col(W)) == 1] <- 1
MoranRes(rnorm(6), W)
}
