% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ess}
\alias{ess}
\title{ess}
\usage{
ess(weights)
}
\arguments{
\item{weights}{Numeric; passed to \code{sum}.}
}
\description{
A step of the smcsam_native implementation. Called by \code{Drvst}, \code{morie_att_weights}, \code{smcsam}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ess(V)
}
