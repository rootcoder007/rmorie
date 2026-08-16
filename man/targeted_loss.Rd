% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{targeted_loss}
\alias{targeted_loss}
\title{targeted_loss}
\usage{
targeted_loss(Q_star, Y)
}
\arguments{
\item{Q_star}{See Usage.}
\item{Y}{See Usage.}
}
\description{
A step of the tlctmle_native implementation. Called by \code{ctmle}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
targeted_loss(V, V)
}
