% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ray_points}
\alias{ray_points}
\title{ray_points}
\usage{
ray_points(origin, direction, t_near, t_far, n_samples, seed = 0, stratified = TRUE)
}
\arguments{
\item{origin}{Passed to \code{.nrfrad_vec}.}
\item{direction}{Passed to \code{.nrfrad_vec}.}
\item{t_near}{See Usage.}
\item{t_far}{See Usage.}
\item{n_samples}{See Usage.}
\item{seed}{Defaults to \code{0}.}
\item{stratified}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{t}, \code{points}, \code{direction}.
}
\keyword{internal}
