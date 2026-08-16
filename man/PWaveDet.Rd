% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PWaveDet}
\alias{PWaveDet}
\title{Hengeveld and van Bemmel, Section 4.3.3.  The P wave is never}
\usage{
PWaveDet(x, qrs, fs, template = NULL)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{template}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
searched for directly: the QRS is deleted, the residue bandpassed 3-11 Hz, ternarised at 50\% and 75\% of the interval maximum, and matched against a ternary template.  Matching a TERNARY version is what makes it tolerant of P-wave amplitude and shape variation.
}
\value{
A list with \code{p}, \code{template}, \code{windows}, \code{bandpass}, \code{fs}, \code{method}.
}
\keyword{internal}
