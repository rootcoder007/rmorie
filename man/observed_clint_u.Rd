% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{observed_clint_u}
\alias{observed_clint_u}
\title{observed_clint_u}
\usage{
observed_clint_u(cl_h, fu_blood, species = "human", qh = NULL, liver_model = "well_stirred")
}
\arguments{
\item{cl_h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fu_blood}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{species}{Defaults to \code{"human"}.}
\item{qh}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{liver_model}{One of \code{"parallel_tube"}, \code{"well_stirred"}. Defaults to \code{"well_stirred"}.}
}
\description{
A step of the clrnt_native implementation. Called by \code{clrnt}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
