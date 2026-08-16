% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannPh}
\alias{HannPh}
\title{Eq (3.107): exactly -omega, slope -1, a constant group delay of one}
\usage{
HannPh(omega)
}
\arguments{
\item{omega}{See Usage.}
}
\description{
sample at every frequency.  Constant group delay is what "no phase distortion" means.
}
\value{
A list with \code{phase}, \code{omega}, \code{group_delay}, \code{slope}, \code{linear_phase}, \code{constant_group_delay}, \code{method}.
}
\keyword{internal}
