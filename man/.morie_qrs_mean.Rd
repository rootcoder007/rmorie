% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_mean}
\alias{.morie_qrs_mean}
\title{.morie_qrs_mean}
\usage{
.morie_qrs_mean(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{CPulseFeat}, \code{EcgFeat}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_qrs_mean(V)
}
