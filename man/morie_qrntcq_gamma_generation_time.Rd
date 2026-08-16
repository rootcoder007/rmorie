% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qrntcq_gamma_generation_time}
\alias{morie_qrntcq_gamma_generation_time}
\title{morie_qrntcq_gamma_generation_time}
\usage{
morie_qrntcq_gamma_generation_time(shape = 2.83, scale = 1.86, grid = NULL, t.max = 30, n = 3001L)
}
\arguments{
\item{shape}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2.83}.}
\item{scale}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.86}.}
\item{grid}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{t.max}{Defaults to \code{30}.}
\item{n}{Defaults to \code{3001L}.}
}
\description{
A step of the qrntcq_native implementation. Called by \code{morie_qrntcq_efficacy_test_and_release}, \code{morie_qrntcq_optimal_duration}, \code{morie_qrntcq_quarantine_efficacy} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{t}, \code{density}.
}
\keyword{internal}
\examples{
morie_qrntcq_gamma_generation_time()
}
