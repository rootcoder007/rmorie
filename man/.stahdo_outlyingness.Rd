% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_outlyingness}
\alias{.stahdo_outlyingness}
\title{.stahdo_outlyingness}
\usage{
.stahdo_outlyingness(X, directions = "subsample", n_directions = 500, seed = 1)
}
\arguments{
\item{X}{See Usage.}
\item{directions}{Defaults to \code{"subsample"}.}
\item{n_directions}{Defaults to \code{500}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
Part of the stahdo_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{outlyingness}, \code{n_directions}, \code{n_used}, \code{exhaustive}, \code{directions}.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
