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
\item{m}{See Usage.}
\item{instruments}{A list; the body reads \code{$instruments} from it.}
\item{form}{Defaults to \code{"sum"}.}
\item{n_equality}{Defaults to \code{0L}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{compare_forms}. See the file header for the source the module follows.
}
\value{
A list with \code{statistic}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
