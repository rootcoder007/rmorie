% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qrntcq_efficacy_test_and_release}
\alias{morie_qrntcq_efficacy_test_and_release}
\title{morie_qrntcq_efficacy_test_and_release}
\usage{
morie_qrntcq_efficacy_test_and_release(t.Q, t.T, t.R, false.negative, generation.time = NULL, t.R.positive = NULL)
}
\arguments{
\item{t.Q}{Passed to \code{morie_qrntcq_quarantine_efficacy}.}
\item{t.T}{Carried through into a list the body builds.}
\item{t.R}{Passed to \code{morie_qrntcq_quarantine_efficacy}.}
\item{false.negative}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{generation.time}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{t.R.positive}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the qrntcq_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{efficacy}, \code{efficacy.detained}, \code{efficacy.released}, \code{false.negative}, \code{t.T}, \code{t.R}, \code{bound}, \code{note}.
}
\keyword{internal}
