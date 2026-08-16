% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{loss}
\alias{loss}
\title{loss}
\usage{
loss(y, y_hat, kind = "square")
}
\arguments{
\item{y}{See Usage.}
\item{y_hat}{See Usage.}
\item{kind}{One of \code{"hinge"}, \code{"log"}, \code{"square"}. Defaults to \code{"square"}.}
}
\description{
A step of the cdaeRC_native implementation. Called by \code{.plcbsc_synthetic_control}, \code{.tlroad_score_spans_eic}, \code{fit_cdae} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
loss(V, V)
}
