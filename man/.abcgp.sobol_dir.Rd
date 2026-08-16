% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.sobol_dir}
\alias{.abcgp.sobol_dir}
\title{.abcgp.sobol_dir}
\usage{
.abcgp.sobol_dir(dim, bits)
}
\arguments{
\item{dim}{A count; the body uses it as \code{seq_len(...)}.}
\item{bits}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.sobol_sequence}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
