% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{svt}
\alias{svt}
\title{svt}
\usage{
svt(M, observed, tau = NULL, step = 1.9, iters = 200L, tol = 1e-06)
}
\arguments{
\item{M}{Passed to \code{.meglt_mat}.}
\item{observed}{Iterated over elementwise, with \code{sapply}.}
\item{tau}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{step}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.9}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-06}.}
}
\description{
A step of the meglt_native implementation. Called by \code{morie_meglt}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{X}, \code{residual_history}, \code{final_residual}, \code{tau}, \code{n_observed}, \code{fraction_observed}, \code{nuclear_norm}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
U <- matrix(rnorm(10), 5, 2)
V <- matrix(rnorm(8), 4, 2)
M <- U \%*\% t(V)
obs <- list()
for (i in 0:4) for (j in 0:3) if (runif(1) < 0.7)
  obs[[length(obs) + 1]] <- c(i, j)
r <- svt(M, obs, tau = 1, iters = 100)
str(r, max.level = 1)
}
