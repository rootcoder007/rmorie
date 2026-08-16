% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_add}
\alias{.groebn_add}
\title{.groebn_add}
\usage{
.groebn_add(f, g)
}
\arguments{
\item{f}{See Usage.}
\item{g}{A vector; indexed elementwise.}
}
\description{
Polynomial arithmetic ---------------------------------------------------------------------------
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.groebn_add(V, V)
}
