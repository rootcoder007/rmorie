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
\item{t_near}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t_far}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_samples}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{stratified}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{t}, \code{points}, \code{direction}.
}
\keyword{internal}
\examples{
r <- ray_points(origin = c(0, 0, 0), direction = c(0, 0, 1),
                t_near = 1, t_far = 5, n_samples = 8, seed = 1,
                stratified = TRUE)
c(length(r$t), nrow(as.matrix(r$points)))
}
