% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{probability_of_control}
\alias{probability_of_control}
\title{probability_of_control}
\usage{
probability_of_control(reps = 200, seed = 0, ...)
}
\arguments{
\item{reps}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{200}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
\item{...}{Passed through.}
}
\description{
A step of the ttrace_native implementation. Called by \code{morie_ttrace}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{probability_of_control}, \code{se}, \code{reps}, \code{median_size}, \code{max_size}, \code{max_cases}, \code{max_weeks}, \code{definition}, \code{method}.
}
\keyword{internal}
\examples{
r <- probability_of_control(reps = 20, seed = 1, R0 = 1.5,
                            trace_prob = 0.8)
str(r, max.level = 1)
}
