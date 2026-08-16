% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_bias_common}
\alias{.fz_bias_common}
\title{.fz_bias_common}
\usage{
.fz_bias_common(t, f_X, transform)
}
\arguments{
\item{t}{See Usage.}
\item{f_X}{See Usage.}
\item{transform}{Passed to \code{.fz_transform}.}
}
\description{
A step of the fauzi_native implementation. Called by \code{morie_fauzi_b1_coefficient}, \code{morie_fauzi_b2_coefficient}, \code{morie_fauzi_b3_coefficient}. See the file header for the source the module follows.
}
\value{
A list with \code{tv}, \code{fx}, \code{tr}, \code{zt}, \code{gp}, \code{gpp}.
}
\keyword{internal}
