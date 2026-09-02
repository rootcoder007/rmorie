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
\item{t.Q}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t.R}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{generation.time}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{t.E}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the qrntcq_native implementation. Called by \code{morie_qrntcq_efficacy_test_and_release}, \code{morie_qrntcq_optimal_duration}, \code{morie_qrntcq_relative_utility}. See the file header for the source the module follows.
}
\value{
A list with \code{efficacy}, \code{prevented.mass}, \code{remaining.mass}, \code{t.Q}, \code{t.R}, \code{max.attainable}, \code{pre.quarantine.mass}.
}
\keyword{internal}
\examples{
morie_qrntcq_quarantine_efficacy(t.Q = 5L, t.R = 5L)
}
