% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{urn_weights}
\alias{urn_weights}
\title{Polya urn weights}
\usage{
urn_weights(counts, alpha)
}
\arguments{
\item{counts}{Numeric vector of positive cluster occupancies.}
\item{alpha}{Concentration, positive.}
}
\description{
Returns the urn\'s mixture weights at the current state. The new-cluster weight is \code{alpha/(alpha+n)} exactly, and is returned separately because it IS the model\'s answer to "how likely is something new".
}
\value{
A list with \code{existing}, \code{new}, \code{n},   \code{K}, \code{total}, \code{note}.
}
\keyword{internal}
