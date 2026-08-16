% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{egcl}
\alias{egcl}
\title{egcl}
\usage{
egcl(H, X, phi_e, phi_x, phi_h, A = NULL, C = NULL, V = NULL, mode = "position", phi_v = NULL, dt = 1)
}
\arguments{
\item{H}{A vector; its length is taken and its elements indexed.}
\item{X}{A vector; indexed elementwise.}
\item{phi_e}{See Usage.}
\item{phi_x}{See Usage.}
\item{phi_h}{See Usage.}
\item{A}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{C}{Defaults to \code{NULL}.}
\item{V}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{mode}{Compared against \code{"position"}. Defaults to \code{"position"}.}
\item{phi_v}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
A step of the egnnL_native implementation. Called by \code{run_egnn}. See the file header for the source the module follows.
}
\value{
A list with \code{H}, \code{X}, \code{V}, \code{messages}.
}
\keyword{internal}
