% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bats_filter}
\alias{bats_filter}
\title{bats_filter}
\usage{
bats_filter(z, spec, theta, x0, long_run_b = 0) 
}
\arguments{
\item{z}{A vector; its length is taken and its elements indexed.}
\item{spec}{A list; the body reads \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_trend} from it.}
\item{theta}{Passed to \code{.unpack}.}
\item{x0}{A vector; indexed elementwise.}
\item{long_run_b}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}, \code{.forecast}, \code{fit_seed_state} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{resid}, \code{fitted}, \code{carry}.
}
\keyword{internal}
\examples{
set.seed(1)
z <- as.numeric(arima.sim(list(ar = 0.5), 40)) + 10
spec <- BatsSpec()
theta <- rep(0.1, n_free(spec))
r <- bats_filter(z, spec, theta, x0 = rep(0, n_states(spec)))
str(r, max.level = 1)
}
