% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.chol}
\alias{.abcgp.chol}
\title{.abcgp.chol}
\usage{
.abcgp.chol(a, jitter = 1e-12)
}
\arguments{
\item{a}{A matrix; indexed by row and column.}
\item{jitter}{Defaults to \code{1e-12}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.draw_mean}, \code{.abcgp.gp_fit}, \code{.abcgp.mvn_logpdf} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
