% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03pnorm}
\alias{.s03pnorm}
\title{.s03pnorm}
\usage{
.s03pnorm(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.huber_k}, \code{Augmn}, \code{Btbca} and 10 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{pnorm}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03pnorm(V)
}
