% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Notch60}
\alias{Notch60}
\title{A conjugate pair of zeros AT the interference frequency, so the gain}
\usage{
Notch60(fs, f0 = 60, z = NULL)
}
\arguments{
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{f0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{60}.}
\item{z}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_polyz}.}
}
\description{
there is exactly nought.  With zeros alone the notch is wide, which is why the book goes on to add poles just inside them.
}
\value{
A list with \code{b}, \code{a}, \code{gain}, \code{zeros}, \code{H}, \code{f0}, \code{fs}, \code{omega_0}, \code{gain_at_the_notch}, \code{dc_gain}, \code{fir}, \code{linear_phase}, \code{notch_is_wide_without_poles}, \code{method}.
}
\keyword{internal}
