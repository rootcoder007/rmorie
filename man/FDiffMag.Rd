% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FDiffMag}
\alias{FDiffMag}
\title{Eq (3.126).  The book prints (2/T) sin(w/2) without bars, right on}
\usage{
FDiffMag(omega, T = 1)
}
\arguments{
\item{omega}{See Usage.}
\item{T}{Defaults to \code{1}.}
}
\description{
0 <= w <= pi, the range plotted; the absolute value is needed outside it.  Largest at Nyquist, which is why the operator amplifies high-frequency noise.
}
\value{
A list with \code{magnitude}, \code{omega}, \code{T}, \code{dc_gain}, \code{nyquist_gain}, \code{roughly_proportional_to_frequency}, \code{book_omits_the_absolute_value}, \code{method}.
}
\keyword{internal}
