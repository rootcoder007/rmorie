% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{inv_box_cox}
\alias{inv_box_cox}
\title{inv_box_cox}
\usage{
inv_box_cox(z, omega)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
\item{omega}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bats_native implementation. Called by \code{morie_bats}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
inv_box_cox(z = c(1, 2, 3, 4, 5, 6, 7, 8), omega = 5L)
}
