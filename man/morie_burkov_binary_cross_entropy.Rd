% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_binary_cross_entropy}
\alias{morie_burkov_binary_cross_entropy}
\title{Binary cross-entropy for one example (Burkov Eq 1.9)}
\usage{
morie_burkov_binary_cross_entropy(y_hat, y)
}
\arguments{
\item{y_hat}{Predicted probabilities in \\[0, 1\\].}
\item{y}{Targets, 0 or 1.}
}
\description{
Binary cross-entropy for one example (Burkov Eq 1.9)
}
\value{
A list with \code{losses}, \code{estimate}, \code{mean_loss}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_burkov_binary_cross_entropy(0.5, 1)
}
