% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qrntcq_quarantine_efficacy}
\alias{morie_qrntcq_quarantine_efficacy}
\title{morie_qrntcq_quarantine_efficacy}
\usage{
morie_qrntcq_quarantine_efficacy(t.Q, t.R, generation.time = NULL, t.E = 0)
}
\arguments{
\item{t.Q}{See Usage.}
\item{t.R}{See Usage.}
\item{generation.time}{Defaults to \code{NULL}.}
\item{t.E}{Defaults to \code{0}.}
}
\description{
Part of the qrntcq_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{efficacy}, \code{prevented.mass}, \code{remaining.mass}, \code{t.Q}, \code{t.R}, \code{max.attainable}, \code{pre.quarantine.mass}.
}
\keyword{internal}
\examples{
morie_qrntcq_quarantine_efficacy(t.Q = 5L, t.R = 5L)
}
