% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Notch}
\alias{Notch}
\title{Notch}
\usage{
Notch(notch_freq, bandwidth = NULL, fs = 1000, r = NULL, z = NULL)
}
\arguments{
\item{notch_freq}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{bandwidth}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1000}.}
\item{r}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_polyz}.}
}
\description{
A step of the rangayyan_filt2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{b}, \code{a}, \code{gain}, \code{H}, \code{f0}, \code{fs}, \code{r}, \code{bandwidth_hz}, \code{omega_0}, \code{zeros}, \code{poles}, \code{gain_at_the_notch}, \code{dc_gain}, \code{iir}, \code{poles_narrow_the_notch}, \code{method}.
}
\keyword{internal}
\examples{
Notch(notch_freq = 60, bandwidth = 5, fs = 1000)
}
