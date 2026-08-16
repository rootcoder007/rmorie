% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsFilt}
\alias{LmsFilt}
\title{LmsFilt}
\usage{
LmsFilt(primary, reference, order = 8, mu = 0.01, variable = FALSE, alpha = 0.02)
}
\arguments{
\item{primary}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{reference}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.}
\item{variable}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{alpha}{Passed to \code{LmsZhang}. Defaults to \code{0.02}.}
}
\description{
A step of the rangayyan_adapt implementation. Called by \code{Anc}. See the file header for the source the module follows.
}
\value{
A list with \code{e}, \code{output}, \code{y}, \code{final_weights}, \code{order}, \code{mu}, \code{variable_step}, \code{step_history}, \code{stable_bound}, \code{within_bound}, \code{input_power}, \code{output_power}, \code{power_reduction}, \code{converges_in_the_mean_only}, \code{method}.
}
\keyword{internal}
\examples{
sine_a <- function(n, cycles, amp = 1, phase = 0) amp * sin(2 * 
    pi * cycles * (0:(n - 1))/n + phase)
n <- 400
v <- sine_a(256, 3)
ref <- sine_a(n, 61)
Anc(v + 0.8 * ref, ref, order = 4, mu = 0.005)
LmsFilt(primary = ref, reference = ref)
}
