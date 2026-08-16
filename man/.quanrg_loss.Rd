% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.quanrg_loss}
\alias{.quanrg_loss}
\title{.quanrg_loss}
\usage{
.quanrg_loss(res, theta)
}
\arguments{
\item{res}{Numeric; combined arithmetically in the body.}
\item{theta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the quanrg_native implementation. Called by \code{morie_quanrg}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.quanrg_loss(V, V)
}
