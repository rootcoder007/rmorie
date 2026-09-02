% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ttrace}
\alias{morie_ttrace}
\title{morie_ttrace}
\usage{
morie_ttrace(reps = 200, seed = 0, ...) 
}
\arguments{
\item{reps}{Passed to \code{probability_of_control}. Defaults to \code{200}.}
\item{seed}{Passed to \code{probability_of_control}. Defaults to \code{0}.}
\item{...}{Passed through.}
}
\description{
A step of the ttrace_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{probability_of_control}.
}
\keyword{internal}
\examples{
r <- morie_ttrace(reps = 20, seed = 1, R0 = 1.5, trace_prob = 0.8)
str(r, max.level = 1)
}
