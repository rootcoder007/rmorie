% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{project_covariance}
\alias{project_covariance}
\title{Project a 3D covariance to a 2D one: J W Sigma W\' J\'}
\usage{
project_covariance(S, W, J) 
}
\arguments{
\item{S}{3D covariance matrix.}
\item{W}{Viewing (or world) matrix.}
\item{J}{Jacobian of the perspective projection.}
}
\description{
Project a 3D covariance to a 2D one: J W Sigma W\' J\'
}
\value{
List with projected, dim, note.
}
\keyword{internal}
\examples{
project_covariance(S = 5L, W = 5L, J = c(1, 2, 3, 4, 5, 6, 7, 8))
}
