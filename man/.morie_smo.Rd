% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_smo}
\alias{.morie_smo}
\title{.morie_smo}
\usage{
.morie_smo(K, y, C = 1, tol = 0.001, max_passes = 50L, max_iter = 10000L, seed = 0L)
}
\arguments{
\item{K}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{C}{Numeric; passed to \code{min}. Defaults to \code{1}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{0.001}.}
\item{max_passes}{Defaults to \code{50L}.}
\item{max_iter}{Defaults to \code{10000L}.}
\item{seed}{Defaults to \code{0L}.}
}
\description{
A step of the esl_native2 implementation. Called by \code{morie_esl_svc}, \code{morie_esl_svm_kernel}. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{b}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
