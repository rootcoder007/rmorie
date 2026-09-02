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
\item{t.Q}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{generation.time}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{t.max}{Numeric; combined arithmetically in the body. Defaults to \code{20}.}
\item{step}{Numeric; combined arithmetically in the body. Defaults to \code{0.25}.}
}
\description{
A step of the qrntcq_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{optimal.t.R}, \code{efficacy.at.optimum}, \code{utility.at.optimum}, \code{curve}, \code{t.Q}, \code{method}.
}
\keyword{internal}
\examples{
morie_qrntcq_optimal_duration()
}
