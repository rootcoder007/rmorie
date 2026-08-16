% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_big_trim}
\alias{.morie_big_trim}
\title{.morie_big_trim}
\usage{
.morie_big_trim(limbs)
}
\arguments{
\item{limbs}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the bigint_native implementation. Called by \code{.morie_big_add_abs}, \code{.morie_big_new}, \code{.morie_big_sub_abs}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_big_trim(V)
}
