% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dice_loss}
\alias{dice_loss}
\title{dice_loss}
\usage{
dice_loss(prob, target)
}
\arguments{
\item{prob}{See Usage.}
\item{target}{See Usage.}
}
\description{
Part of the samdec_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loss}, \code{dice}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
dice_loss(V, V)
}
