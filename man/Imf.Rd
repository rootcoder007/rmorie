% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Imf}
\alias{Imf}
\title{One IMF plus its admissibility evidence, Sec 9.4 and eqs (9.8)-(9.11)}
\usage{
Imf(x, max_iter = 50, tol = 0.05)
}
\arguments{
\item{x}{See Usage.}
\item{max_iter}{Defaults to \code{50}.}
\item{tol}{Defaults to \code{0.05}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{imf}, \code{residual}, \code{n_extrema}, \code{n_zero_crossings}, \code{extrema_zerox_ok}, \code{mean_envelope}, \code{max_envelope_mean}, \code{envelope_mean_ok}, \code{is_imf}, \code{iterations}, \code{converged}, \code{amplitude}, \code{phase}, \code{method}.
}
\keyword{internal}
