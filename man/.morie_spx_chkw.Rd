% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_chkw}
\alias{.morie_spx_chkw}
\title{.morie_spx_chkw}
\usage{
.morie_spx_chkw(w, n, zero_diag = TRUE)
}
\arguments{
\item{w}{A matrix; passed to \code{as.matrix}.}
\item{n}{See Usage.}
\item{zero_diag}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the sp_fill implementation. Called by \code{LisaClust}, \code{LisaI}, \code{MantelM2} and 7 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{W}, as built in the body.
}
\keyword{internal}
