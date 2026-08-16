% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_glu}
\alias{.morie_jo_glu}
\title{.morie_jo_glu}
\usage{
.morie_jo_glu(gamma, w4, b4, w5, b5)
}
\arguments{
\item{gamma}{Passed to \code{.morie_jo_matvec}.}
\item{w4}{Passed to \code{.morie_jo_matvec}.}
\item{b4}{Numeric; combined arithmetically in the body.}
\item{w5}{Passed to \code{.morie_jo_matvec}.}
\item{b5}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ts_joseph implementation. Called by \code{morie_tftnet}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
