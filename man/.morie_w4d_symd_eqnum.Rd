% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_symd_eqnum}
\alias{.morie_w4d_symd_eqnum}
\title{.morie_w4d_symd_eqnum}
\usage{
.morie_w4d_symd_eqnum(t, v)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{v}{Passed to \code{all.equal}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{.morie_w4d_symd_simplify}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_w4d_symd_eqnum(V, V)
}
