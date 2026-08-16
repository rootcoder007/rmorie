% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causdidwd_with_intercept}
\alias{.causdidwd_with_intercept}
\title{.causdidwd_with_intercept}
\usage{
.causdidwd_with_intercept(rows)
}
\arguments{
\item{rows}{See Usage.}
}
\description{
A step of the causdidwd_native implementation. Called by \code{morie_two_way_fixed_effects}, \code{morie_two_way_mundlak}. See the file header for the source the module follows.
}
\value{
The value of \code{cbind}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.causdidwd_with_intercept(V)
}
