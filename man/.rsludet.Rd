% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsludet}
\alias{.rsludet}
\title{.rsludet}
\usage{
.rsludet(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{.rscovdet}. See the file header for the source the module follows.
}
\value{
The value of \code{d}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.rsludet(M)
}
