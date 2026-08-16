% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qrntcq_optimal_duration}
\alias{morie_qrntcq_optimal_duration}
\title{morie_qrntcq_optimal_duration}
\usage{
morie_qrntcq_optimal_duration(t.Q = 3, generation.time = NULL, t.max = 20, step = 0.25)
}
\arguments{
\item{t.Q}{Defaults to \code{3}.}
\item{generation.time}{Defaults to \code{NULL}.}
\item{t.max}{Defaults to \code{20}.}
\item{step}{Defaults to \code{0.25}.}
}
\description{
Part of the qrntcq_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{optimal.t.R}, \code{efficacy.at.optimum}, \code{utility.at.optimum}, \code{curve}, \code{t.Q}, \code{method}.
}
\keyword{internal}
\examples{
morie_qrntcq_optimal_duration()
}
