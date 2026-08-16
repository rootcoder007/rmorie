% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lbfgsm_dot}
\alias{.lbfgsm_dot}
\title{.lbfgsm_dot}
\usage{
.lbfgsm_dot(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the lbfgsm_native implementation. Called by \code{morie_lbfgsm}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.lbfgsm_dot(V, V)
}
