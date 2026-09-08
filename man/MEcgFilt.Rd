% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MEcgFilt}
\alias{MEcgFilt}
\title{Sections 3.3.5 and 9.7.2: the thoracic lead is the reference input of}
\usage{
MEcgFilt(abd, thor, order = 16, mu = 0.01) 
}
\arguments{
\item{abd}{A vector; its length is taken and its elements indexed.}
\item{thor}{A vector; its length is taken and its elements indexed.}
\item{order}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{16}.}
\item{mu}{Numeric; combined arithmetically in the body. Defaults to \code{0.01}.}
}
\description{
an adaptive noise canceller whose primary input is the abdominal lead, so the residual error is the fetal ECG.  Normalised LMS, weights start at zero, no random initialisation.
}
\value{
A list with \code{fetal}, \code{maternal}, \code{weights}, \code{order}, \code{mu}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(8)
n <- 800
mecg <- sin(2 * pi * 2 * (1:n) / 200)
thor <- sin(2 * pi * 1.2 * (1:n) / 200)
abd <- 0.6 * thor + 0.3 * mecg + rnorm(n, 0, 0.05)
MEcgFilt(abd, thor)
}
