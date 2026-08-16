% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ks_statistic}
\alias{ks_statistic}
\title{ks_statistic}
\usage{
ks_statistic(m, instruments, form = "sum", n_equality = 0L)
}
\arguments{
\item{m}{Passed to \code{weighted_moments}.}
\item{instruments}{A list; the body reads \code{$instruments} from it.}
\item{form}{Carried through into a list the body builds. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{S_function}. Defaults to \code{0L}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{compare_forms}, \code{ks_confidence_set}. See the file header for the source the module follows.
}
\value{
A list with \code{statistic}, \code{argmax}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(runif(60), 30, 2)
inst <- hypercube_instruments(X, n_levels = 2L)
r <- ks_statistic(X[, 1] - 0.4, inst)
str(r, max.level = 1)
}
