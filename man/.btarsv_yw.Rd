% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.btarsv_yw}
\alias{.btarsv_yw}
\title{.btarsv_yw}
\usage{
.btarsv_yw(xc, p)
}
\arguments{
\item{xc}{A vector; its length is taken and its elements indexed.}
\item{p}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
A step of the btarsv_native implementation. Called by \code{morie_btarsv}. See the file header for the source the module follows.
}
\value{
A list with \code{phi}, \code{v}.
}
\keyword{internal}
\examples{
.btarsv_yw(xc = c(1, 2, 3, 4, 5, 6, 7, 8), p = 0.5)
}
