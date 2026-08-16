% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cvm_statistic}
\alias{cvm_statistic}
\title{cvm_statistic}
\usage{
cvm_statistic(m, instruments, form = "sum", n_equality = 0L, weights = NULL)
}
\arguments{
\item{m}{Passed to \code{weighted_moments}.}
\item{instruments}{A list; the body reads \code{$instruments} from it.}
\item{form}{Carried through into a list the body builds. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{S_function}. Defaults to \code{0L}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{compare_forms}. See the file header for the source the module follows.
}
\value{
A list with \code{statistic}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(runif(60), 30, 2)
inst <- hypercube_instruments(X, n_levels = 2L)
r <- cvm_statistic(X[, 1] - 0.4, inst)
str(r, max.level = 1)
}
