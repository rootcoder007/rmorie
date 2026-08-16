% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.crt_egcd}
\alias{.crt_egcd}
\title{Extended Euclid: returns (g, c, d) with c*a + d*b = g}
\usage{
.crt_egcd(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
A step of the crtT_mixedcase_native implementation. Called by \code{crtT}. See the file header for the source the module follows.
}
\value{
A list with \code{g}, \code{c}, \code{d}.
}
\keyword{internal}
