% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sem}
\alias{Sem}
\title{Section 8.5.1, the spectral error measure: the mean squared}
\usage{
Sem(psd, reference)
}
\arguments{
\item{psd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{reference}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
difference of the LOG spectra.  Taking logs is what makes it scale-free -- a pure gain change shifts every log bin by the same constant, so it lands entirely in mean_offset and leaves shape_only at zero.
}
\value{
A list with \code{sem}, \code{log_difference}, \code{n_bins}, \code{mean_offset}, \code{shape_only}, \code{gain_change_only}, \code{zero_bins}, \code{scale_free}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Sem(V, V)
}
