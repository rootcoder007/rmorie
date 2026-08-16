% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_nmf}
\alias{.morie_w4c_nmf}
\title{Lee-Seung multiplicative-update NMF; LCG-seeded uniform(0, 1) init}
\usage{
.morie_w4c_nmf(X, k, max_iter, tol, seed)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{k}{A count; the body uses it as \code{matrix(...)}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}.}
\item{tol}{Passed to \code{<}.}
\item{seed}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_nmf}. See the file header for the source the module follows.
}
\value{
A list with \code{W}, \code{H}, \code{n_iter}.
}
\keyword{internal}
\examples{
.morie_w4c_nmf(X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), k = 5L, max_iter = c(1, 2, 3, 4, 5, 6, 7, 8), tol = 0.5, seed = 5L)
}
