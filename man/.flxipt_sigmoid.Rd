% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_sigmoid}
\alias{.flxipt_sigmoid}
\title{Sigmoid --------------------------------------------------------}
\usage{
.flxipt_sigmoid(v)
}
\arguments{
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the flxipt_native implementation. Called by \code{.flxipt_fit}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.flxipt_sigmoid(V)
}
