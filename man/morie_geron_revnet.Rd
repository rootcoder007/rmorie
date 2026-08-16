% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_revnet}
\alias{morie_geron_revnet}
\title{RevNet: reversible residual block, verified by explicit inversion (Geron Ch 12, hmrvn)}
\usage{
morie_geron_revnet(x, F, G)
}
\arguments{
\item{x}{Input, even width on last axis.}
\item{F,G}{Shape-preserving residual functions.}
}
\description{
RevNet: reversible residual block, verified by explicit inversion (Geron Ch 12, hmrvn)
}
\value{
A list with \code{y}, \code{y1}, \code{y2}, \code{x1}, \code{x2}, \code{x_reconstructed}, \code{reconstruction_error}, \code{reversible}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
