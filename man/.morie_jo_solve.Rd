% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_solve}
\alias{.morie_jo_solve}
\title{.morie_jo_solve}
\usage{
.morie_jo_solve(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{A vector; its length is taken.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_ols}, \code{morie_adfur}, \code{morie_quantreg}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
