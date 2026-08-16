% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_zerox}
\alias{.tf_zerox}
\title{.tf_zerox}
\usage{
.tf_zerox(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Imf}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_zerox(V)
}
