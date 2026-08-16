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
\item{zeros_in}{See Usage.}
\item{zeros_out}{See Usage.}
\item{poles_in}{See Usage.}
\item{poles_out}{See Usage.}
\item{nmax}{Defaults to \code{32}.}
\item{constant}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_cep implementation; see the file header for the source it follows.
}
\value{
A list with \code{alpha}, \code{K}, \code{bound}, \code{quefrency}, \code{decays_at_least_as_one_over_n}, \code{near_unit_circle}, \code{method}.
}
\keyword{internal}
\examples{
CCepDecay(zeros_in = c(1, 2, 3, 4, 5, 6, 7, 8), zeros_out = c(1, 2, 3, 4, 5, 6, 7, 8), poles_in = c(1, 2, 3, 4, 5, 6, 7, 8), poles_out = c(1, 2, 3, 4, 5, 6, 7, 8))
}
