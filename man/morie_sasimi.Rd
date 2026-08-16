% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sasimi}
\alias{morie_sasimi}
\title{morie_sasimi}
\usage{
morie_sasimi(fp_a, fp_b, coefficient = "tanimoto", alpha = NULL, beta = NULL)
}
\arguments{
\item{fp_a}{See Usage.}
\item{fp_b}{See Usage.}
\item{coefficient}{Defaults to \code{"tanimoto"}.}
\item{alpha}{Defaults to \code{NULL}.}
\item{beta}{Defaults to \code{NULL}.}
}
\description{
Part of the sasimi_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{similarity}, \code{distance}, \code{bits_a}, \code{bits_b}, \code{bits_shared}, \code{coefficient}, \code{method}.
}
\references{
----------
Jaccard, P. (1912) "The distribution of the flora in the alpine
zone", *New Phytologist* 11(2), 37-50,
doi:10.1111/j.1469-8137.1912.tb05611.x, for the coefficient
:math:`c/(a+b-c)` itself.

Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
similarity searching", *Journal of Chemical Information and Computer
Sciences* 38(6), 983-996, doi:10.1021/ci9800211. The survey these
formulae are taken from: the Tanimoto, Dice, cosine and Tversky
coefficients on binary fingerprints, the relations between them, and
the size bias of Tanimoto.
"""
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sasimi(V, V)
}
