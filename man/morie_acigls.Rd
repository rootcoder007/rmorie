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
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{H}{See Usage.}
\item{cluster}{See Usage.}
\item{small_sample}{Defaults to \code{TRUE}.}
}
\description{
Part of the acigls_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{std_errors}, \code{naive_std_errors}, \code{vcov}, \code{residuals}, \code{n}, \code{n_clusters}, \code{n_coefficients}, \code{sum_weights}, \code{finite_sample_correction}, \code{inflation}, \code{method}.
}
\keyword{internal}
\examples{
morie_acigls(y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), H = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
}
