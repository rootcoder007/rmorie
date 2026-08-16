% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w508_llr_side}
\alias{.morie_w508_llr_side}
\title{.morie_w508_llr_side}
\usage{
.morie_w508_llr_side(dm, ym, w)
}
\arguments{
\item{dm}{A vector; its length is taken.}
\item{ym}{A matrix; passed to \code{\%*\%}.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rdd_w508_native implementation. Called by \code{Causrdd}. See the file header for the source the module follows.
}
\value{
A list with \code{a}, \code{b}, \code{v}.
}
\keyword{internal}
\examples{
.morie_w508_llr_side(dm = c(1, 2, 3, 4, 5, 6, 7, 8), ym = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
