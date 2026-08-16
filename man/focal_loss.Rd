% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{focal_loss}
\alias{focal_loss}
\title{focal_loss}
\usage{
focal_loss(prob, target, gamma = 2, alpha = 0.25)
}
\arguments{
\item{prob}{See Usage.}
\item{target}{See Usage.}
\item{gamma}{Defaults to \code{2}.}
\item{alpha}{Defaults to \code{0.25}.}
}
\description{
Part of the samdec_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loss}, \code{modulating}, \code{gamma}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
focal_loss(V, V)
}
