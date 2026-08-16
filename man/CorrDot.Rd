% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CorrDot}
\alias{CorrDot}
\title{Eq (4.25).  Delegates to DotProd -- one copy of the arithmetic}
\usage{
CorrDot(x, y)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
}
\description{
Part of the rangayyan_ccf implementation; see the file header for the source it follows.
}
\value{
A list with \code{gamma}, \code{dot_product}, \code{norms}, \code{pearson}, \code{is_a_cosine_not_an_agreement}, \code{unity_for_a_positive_rescaling}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CorrDot(V, V)
}
