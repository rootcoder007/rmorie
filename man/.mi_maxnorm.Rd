% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mi_maxnorm}
\alias{.mi_maxnorm}
\title{.mi_maxnorm}
\usage{
.mi_maxnorm(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the miest1_native implementation. Called by \code{morie_miest1}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mi_maxnorm(V, V)
}
