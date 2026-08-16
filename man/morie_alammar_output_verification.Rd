% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_output_verification}
\alias{morie_alammar_output_verification}
\title{morie_alammar_output_verification}
\usage{
morie_alammar_output_verification(response, criteria, verifier_model)
}
\arguments{
\item{response}{Text under test.}
\item{criteria}{Character vector.}
\item{verifier_model}{function(response, criterion) -> "PASS"|"FAIL".}
}
\description{
morie_alammar_output_verification
}
\value{
A list with \code{passed}, \code{verdicts}, \code{failed_criteria}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
