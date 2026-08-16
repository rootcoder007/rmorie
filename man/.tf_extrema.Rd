% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_extrema}
\alias{.tf_extrema}
\title{.tf_extrema}
\usage{
.tf_extrema(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_emd}, \code{.tf_sift}, \code{Imf}. See the file header for the source the module follows.
}
\value{
A list with \code{mx}, \code{mn}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_extrema(V)
}
