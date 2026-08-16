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
\item{abd}{See Usage.}
\item{thor}{See Usage.}
\item{order}{Defaults to \code{16}.}
\item{mu}{Defaults to \code{0.01}.}
}
\description{
an adaptive noise canceller whose primary input is the abdominal lead, so the residual error is the fetal ECG.  Normalised LMS, weights start at zero, no random initialisation.
}
\value{
A list with \code{fetal}, \code{maternal}, \code{weights}, \code{order}, \code{mu}, \code{n}, \code{method}.
}
\keyword{internal}
