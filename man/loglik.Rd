% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{loglik}
\alias{loglik}
\title{loglik}
\usage{
loglik(w, ar = numeric(0), ma = numeric(0))
}
\arguments{
\item{w}{A vector; its length is taken and its elements indexed.}
\item{ar}{Passed to \code{.sarima_state_space}. Defaults to \code{numeric(0)}.}
\item{ma}{Passed to \code{.sarima_state_space}. Defaults to \code{numeric(0)}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.gwasem_reml_delta}, \code{.sarima_fit}, \code{morie_glm_nb} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{sigma2}, \code{n}, \code{exact_ssq}, \code{sum_log_f}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
loglik(V)
}
