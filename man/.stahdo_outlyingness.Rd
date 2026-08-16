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
\item{X}{Passed to \code{.stahdo_prep}.}
\item{directions}{Compared against \code{"subsample"}. Defaults to \code{"subsample"}.}
\item{n_directions}{Passed to \code{.stahdo_subsample_dirs}. Defaults to \code{500}.}
\item{seed}{Passed to \code{.stahdo_subsample_dirs}. Defaults to \code{1}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_stahel_donoho}. See the file header for the source the module follows.
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
