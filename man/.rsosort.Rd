% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsosort}
\alias{.rsosort}
\title{Indices that sort v ascending, ties broken by the lower index.}
\usage{
.rsosort(v)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
}
\description{
Written out rather than delegated to order() because the C-step selects "the h smallest distances": which of two tied points is taken changes the subset.
}
\value{
The value of \code{idx}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rsosort(V)
}
