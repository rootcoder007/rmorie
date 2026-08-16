% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_standardise}
\alias{.propinf_standardise}
\title{.propinf_standardise}
\usage{
.propinf_standardise(feats)
}
\arguments{
\item{feats}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}. See the file header for the source the module follows.
}
\value{
A list with \code{feats}, \code{mu}, \code{sd}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.propinf_standardise(V)
}
