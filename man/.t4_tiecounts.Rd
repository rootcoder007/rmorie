% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_tiecounts}
\alias{.t4_tiecounts}
\title{.t4_tiecounts}
\usage{
.t4_tiecounts(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the tail4_core implementation. Called by \code{.t4_kendalltaub}, \code{Mktest}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t4_tiecounts(V)
}
