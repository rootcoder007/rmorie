% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.sobol_sequence}
\alias{.abcgp.sobol_sequence}
\title{.abcgp.sobol_sequence}
\usage{
.abcgp.sobol_sequence(n, dim, skip = 0)
}
\arguments{
\item{n}{A count; the body uses it as \code{matrix(...)}.}
\item{dim}{A count; the body uses it as \code{seq_len(...)}.}
\item{skip}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.design_from_prior}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
