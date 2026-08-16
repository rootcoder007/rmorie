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
\item{x}{Passed to \code{.tf_need}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{imf}, \code{residual}, \code{n_extrema}, \code{n_zero_crossings}, \code{extrema_zerox_ok}, \code{mean_envelope}, \code{max_envelope_mean}, \code{envelope_mean_ok}, \code{is_imf}, \code{iterations}, \code{converged}, \code{amplitude}, \code{phase}, \code{method}.
}
\keyword{internal}
\examples{
Imf(x = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5))
}
