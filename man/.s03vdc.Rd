% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03vdc}
\alias{.s03vdc}
\title{Van der Corput point -- the deterministic stand-in for a uniform draw}
\usage{
.s03vdc(i, base = 2L)
}
\arguments{
\item{i}{See Usage.}
\item{base}{Numeric; combined arithmetically in the body. Defaults to \code{2L}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.bt_counts}, \code{.rfcand}, \code{.s03mammen} and 22 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
