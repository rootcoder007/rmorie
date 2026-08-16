% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{covariance_from_scale_rotation}
\alias{covariance_from_scale_rotation}
\title{Build a covariance matrix from a scale vector and a quaternion}
\usage{
covariance_from_scale_rotation(scale, quaternion)
}
\arguments{
\item{scale}{Positive length-3 scale vector.}
\item{quaternion}{Length-4 rotation quaternion.}
}
\description{
Build a covariance matrix from a scale vector and a quaternion
}
\value{
List with covariance, rotation, scale, note.
}
\keyword{internal}
