% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sasimi_nearest_neighbours}
\alias{sasimi_nearest_neighbours}
\title{sasimi_nearest_neighbours}
\usage{
sasimi_nearest_neighbours(query, fps, k = 5L, coefficient = "tanimoto") 
}
\arguments{
\item{query}{Passed to \code{sasimi_fingerprint}.}
\item{fps}{A vector; its length is taken and its elements indexed.}
\item{k}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{coefficient}{Passed to \code{.sasimi_coef}. Defaults to \code{"tanimoto"}.}
}
\description{
A step of the sasimi_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\references{
----------
Jaccard, P. (1912) "The distribution of the flora in the alpine
zone", *New Phytologist* 11(2), 37-50,
doi:10.1111/j.1469-8137.1912.tb05611.x, for the coefficient
\eqn{c/(a+b-c)} itself.

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
sasimi_nearest_neighbours(V, V)
}
