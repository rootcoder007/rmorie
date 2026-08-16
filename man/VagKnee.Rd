% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VagKnee}
\alias{VagKnee}
\title{VagKnee: linear-discriminant screening of a VAG signal for knee}
\usage{
VagKnee(vag, fs, weights = NULL, bias = NULL, n_segments = 8)
}
\arguments{
\item{vag}{Passed to \code{VagFeat}.}
\item{fs}{Passed to \code{VagFeat}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{bias}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_segments}{Passed to \code{VagFeat}. Defaults to \code{8}.}
}
\description{
cartilage pathology.
}
\value{
The value of \code{base}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(6)
fs <- 2000
vag <- rnorm(fs, 0, seq(0.5, 2, length.out = fs))
r <- VagKnee(vag, fs = fs)
str(r, max.level = 1)
}
