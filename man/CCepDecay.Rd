% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CCepDecay}
\alias{CCepDecay}
\title{CCepDecay}
\usage{
CCepDecay(zeros_in, zeros_out, poles_in, poles_out, nmax = 32, constant = NULL)
}
\arguments{
\item{zeros_in}{Coerced to complex by the body, with \code{as.complex}.}
\item{zeros_out}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles_in}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles_out}{Coerced to complex by the body, with \code{as.complex}.}
\item{nmax}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32}.}
\item{constant}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_cep implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{K}, \code{bound}, \code{quefrency}, \code{decays_at_least_as_one_over_n}, \code{near_unit_circle}, \code{method}.
}
\keyword{internal}
\examples{
CCepDecay(zeros_in = c(1, 2, 3, 4, 5, 6, 7, 8), zeros_out = c(1, 2, 3, 4, 5, 6, 7, 8), poles_in = c(1, 2, 3, 4, 5, 6, 7, 8), poles_out = c(1, 2, 3, 4, 5, 6, 7, 8))
}
