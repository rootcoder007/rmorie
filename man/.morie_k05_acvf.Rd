% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_acvf}
\alias{.morie_k05_acvf}
\title{.morie_k05_acvf}
\usage{
.morie_k05_acvf(v, n, max_lag)
}
\arguments{
\item{v}{Numeric; passed to \code{mean}.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{max_lag}{Passed to \code{:}.}
}
\description{
A step of the k05_tranche1 implementation. Called by \code{morie_sample_acf}, \code{morie_sample_pacf}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.morie_k05_acvf(v = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, max_lag = c(1, 2, 3, 4, 5, 6, 7, 8))
}
