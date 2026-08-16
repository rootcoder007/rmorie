% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FetalEcg}
\alias{FetalEcg}
\title{FetalEcg}
\usage{
FetalEcg(abdominal, chest, order = 32, mu = 0.005, method = "lms")
}
\arguments{
\item{abdominal}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{chest}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Carried through into a list the body builds. Defaults to \code{32}.}
\item{mu}{Passed to \code{Anc}. Defaults to \code{0.005}.}
\item{method}{Passed to \code{Anc}. Defaults to \code{"lms"}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{fetal}, \code{maternal_estimate}, \code{order}, \code{input_power}, \code{output_power}, \code{suppression_db}, \code{reference_leakage}, \code{single_reference}, \code{widrow_used_multiple_references}, \code{method}.
}
\keyword{internal}
\examples{
sine_a <- function(n, cycles, amp = 1, phase = 0) amp * sin(2 * 
    pi * cycles * (0:(n - 1))/n + phase)
n <- 400
v <- sine_a(256, 3)
ref <- sine_a(n, 61)
FetalEcg(v + 0.8 * ref, ref, order = 4, mu = 0.005)
}
