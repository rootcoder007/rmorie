% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rdd_side}
\alias{.mor_rdd_side}
\title{Weighted linear fit on one side, with the HC0 sandwich variance of}
\usage{
.mor_rdd_side(dm, ym, w)
}
\arguments{
\item{dm}{See Usage.}
\item{ym}{A matrix; passed to \code{\%*\%}.}
\item{w}{A count; the body uses it as \code{rep(...)}.}
}
\description{
the intercept (the boundary value that the RDD contrast uses)
}
\value{
A list with \code{a}, \code{b}, \code{v}.
}
\keyword{internal}
