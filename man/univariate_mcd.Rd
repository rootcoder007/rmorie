% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{univariate_mcd}
\alias{univariate_mcd}
\title{univariate_mcd}
\usage{
univariate_mcd(values, h = NULL, consistent = TRUE)
}
\arguments{
\item{values}{See Usage.}
\item{h}{Defaults to \code{NULL}.}
\item{consistent}{Defaults to \code{TRUE}.}
}
\description{
Part of the robpca_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
univariate_mcd(V)
}
