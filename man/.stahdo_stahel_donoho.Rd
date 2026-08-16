% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_stahel_donoho}
\alias{.stahdo_stahel_donoho}
\title{.stahdo_stahel_donoho}
\usage{
.stahdo_stahel_donoho(X, directions = "subsample", n_directions = 500, seed = 1, cutoff = NULL)
}
\arguments{
\item{X}{Passed to \code{.stahdo_prep}.}
\item{directions}{Passed to \code{.stahdo_outlyingness}. Defaults to \code{"subsample"}.}
\item{n_directions}{Passed to \code{.stahdo_outlyingness}. Defaults to \code{500}.}
\item{seed}{Passed to \code{.stahdo_outlyingness}. Defaults to \code{1}.}
\item{cutoff}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{morie_stahdo}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{location}, \code{scatter}, \code{outlyingness}, \code{weights}, \code{cutoff}, \code{n_directions}, \code{n_used}, \code{exhaustive}, \code{directions}, \code{n_downweighted}, \code{n}, \code{p}, \code{method}.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.stahdo_stahel_donoho(D)
}
