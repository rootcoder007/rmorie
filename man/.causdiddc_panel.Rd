% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causdiddc_panel}
\alias{.causdiddc_panel}
\title{.causdiddc_panel}
\usage{
.causdiddc_panel(Y, D, group, period)
}
\arguments{
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{D}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{group}{Coerced to vector by the body, with \code{as.vector}.}
\item{period}{Coerced to vector by the body, with \code{as.vector}.}
}
\description{
A step of the causdiddc_native implementation. Called by \code{.causdiddc_did_m}, \code{.causdiddc_twfe}. See the file header for the source the module follows.
}
\value{
A list with \code{Y}, \code{D}, \code{g}, \code{t}, \code{n}.
}
\keyword{internal}
