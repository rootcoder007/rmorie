% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{classification_features}
\alias{classification_features}
\title{classification_features}
\usage{
classification_features(u, v) 
}
\arguments{
\item{u}{Passed to \code{.sbert_vec}.}
\item{v}{Passed to \code{.sbert_vec}.}
}
\description{
A step of the sbert_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{features}, \code{u}, \code{v}, \code{abs_diff}, \code{dim}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
classification_features(V, V)
}
