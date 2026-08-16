% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ecfp_envkey}
\alias{.ecfp_envkey}
\title{.ecfp_envkey}
\usage{
.ecfp_envkey(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the ecfp4 implementation. Called by \code{.ecfp_morgan}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ecfp_envkey(V)
}
