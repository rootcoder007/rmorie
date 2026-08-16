% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rkmeans_spatial_median}
\alias{.rkmeans_spatial_median}
\title{.rkmeans_spatial_median}
\usage{
.rkmeans_spatial_median(pts, tol = 1e-10, max_iter = 200)
}
\arguments{
\item{pts}{Passed to \code{.rkmeans_mean}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-10}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
}
\description{
A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rkmeans_spatial_median(V)
}
