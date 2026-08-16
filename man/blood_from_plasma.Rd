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
\item{cl_plasma}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fu_plasma}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{blood_plasma_ratio}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{charge}{One of \code{"acidic"}, \code{"basic"}, \code{"neutral"}. Defaults to \code{"neutral"}.}
}
\description{
A step of the clrnt_native implementation. Called by \code{clrnt}. See the file header for the source the module follows.
}
\value{
A list with \code{cl_blood}, \code{fu_blood}, \code{rb}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
blood_from_plasma(V, V)
}
