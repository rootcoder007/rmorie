% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{blood_from_plasma}
\alias{blood_from_plasma}
\title{blood_from_plasma}
\usage{
blood_from_plasma(cl_plasma, fu_plasma, blood_plasma_ratio = NULL, charge = "neutral")
}
\arguments{
\item{cl_plasma}{See Usage.}
\item{fu_plasma}{See Usage.}
\item{blood_plasma_ratio}{Defaults to \code{NULL}.}
\item{charge}{Defaults to \code{"neutral"}.}
}
\description{
Part of the clrnt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{cl_blood}, \code{fu_blood}, \code{rb}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
blood_from_plasma(V, V)
}
