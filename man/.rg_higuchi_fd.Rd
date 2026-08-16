% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rg_higuchi_fd}
\alias{.rg_higuchi_fd}
\title{.rg_higuchi_fd}
\usage{
.rg_higuchi_fd(x, kmax)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{kmax}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rgemgfd implementation. Called by \code{rgemgfd}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
.rg_higuchi_fd(x = c(1, 2, 3, 4, 5, 6, 7, 8), kmax = 5L)
}
