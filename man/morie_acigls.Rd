% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_acigls}
\alias{morie_acigls}
\title{morie_acigls}
\usage{
morie_acigls(y, A, H, cluster, small_sample = TRUE)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{A}{A matrix; indexed by row and column.}
\item{H}{See Usage.}
\item{cluster}{See Usage.}
\item{small_sample}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the acigls_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{std_errors}, \code{naive_std_errors}, \code{vcov}, \code{residuals}, \code{n}, \code{n_clusters}, \code{n_coefficients}, \code{sum_weights}, \code{finite_sample_correction}, \code{inflation}, \code{method}.
}
\keyword{internal}
\examples{
morie_acigls(y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), H = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
}
