% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hrz_nw}
\alias{.morie_hrz_nw}
\title{Nadaraya-Watson fitted values on a supplied grid}
\usage{
.morie_hrz_nw(x, y, grid, h)
}
\arguments{
\item{x}{See Usage.}
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{grid}{See Usage.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the horowitz_native8 implementation. Called by \code{morie_direct_discrete}, \code{morie_nls_weight_function}, \code{morie_one_step_efficient}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
