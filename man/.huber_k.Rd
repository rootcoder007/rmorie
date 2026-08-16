% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.huber_k}
\alias{.huber_k}
\title{Solve 2 phi(k)/k - 2 Phi(-k) = eps/(1-eps) for k, by bisection}
\usage{
.huber_k(eps)
}
\arguments{
\item{eps}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the contam implementation. Called by \code{Contam}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.huber_k(eps = 0.5)
}
