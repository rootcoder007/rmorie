% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rkmeans_huber_centre}
\alias{.rkmeans_huber_centre}
\title{.rkmeans_huber_centre}
\usage{
.rkmeans_huber_centre(pts, c_val, tol = 1e-10, max_iter = 200)
}
\arguments{
\item{pts}{Passed to \code{.rkmeans_mean}.}
\item{c_val}{Numeric; combined arithmetically in the body.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
}
\description{
A step of the rkmeans_native implementation. Called by \code{.rkmeans_concentrate}. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
