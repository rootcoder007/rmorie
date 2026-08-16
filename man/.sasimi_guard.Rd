% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sasimi_guard}
\alias{.sasimi_guard}
\title{.sasimi_guard}
\usage{
.sasimi_guard(n)
}
\arguments{
\item{n}{A list; the body reads \code{$a}, \code{$b} from it.}
}
\description{
A step of the sasimi_native implementation. Called by \code{sasimi_cosine}, \code{sasimi_dice}, \code{sasimi_tanimoto} and 1 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
