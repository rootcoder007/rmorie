% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_did_first}
\alias{.mor_did_first}
\title{First treated period index (0-based) per unit; Inf if never treated}
\usage{
.mor_did_first(D, unit, time, units = NULL, periods = NULL)
}
\arguments{
\item{D}{Passed to \code{.mor_did_panel}.}
\item{unit}{Passed to \code{.mor_did_panel}.}
\item{time}{Passed to \code{.mor_did_panel}.}
\item{units}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{periods}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the cssant_native implementation. Called by \code{morie_boryis}, \code{morie_cssant}. See the file header for the source the module follows.
}
\value{
A list with \code{g}, \code{Dm}, \code{units}, \code{periods}.
}
\keyword{internal}
