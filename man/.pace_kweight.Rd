% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pace_kweight}
\alias{.pace_kweight}
\title{morie_pace}
\usage{
.pace_kweight(u, kernel)
}
\arguments{
\item{u}{Scaled distance.}
\item{kernel}{\code{"epan"} or \code{"gauss"}.}
}
\description{
Internal: kernel weight for the PACE smoothers Epanechnikov on \eqn{|u| \le 1}, or the Gaussian kernel without its normalising constant -- the constant cancels in a local linear fit, and leaving it out is what the Python arm does.
}
\value{
A numeric weight.
}
\keyword{internal}
