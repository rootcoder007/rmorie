% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ehh_curve}
\alias{.mor_ehh_curve}
\title{.mor_ehh_curve}
\usage{
.mor_ehh_curve(H, core, carriers)
}
\arguments{
\item{H}{A matrix; indexed by row and column.}
\item{core}{Numeric; passed to \code{max}.}
\item{carriers}{A vector; its length is taken.}
}
\description{
A step of the ehhdec_native implementation. Called by \code{morie_ehhdec}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
