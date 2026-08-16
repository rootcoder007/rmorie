% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_stahdo}
\alias{morie_stahdo}
\title{morie_stahdo}
\usage{
morie_stahdo(X, directions = "subsample", n_directions = 500, seed = 1, cutoff = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{directions}{Defaults to \code{"subsample"}.}
\item{n_directions}{Defaults to \code{500}.}
\item{seed}{Defaults to \code{1}.}
\item{cutoff}{Defaults to \code{NULL}.}
}
\description{
Part of the stahdo_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.stahdo_stahel_donoho}.
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
morie_stahdo(D)
}
