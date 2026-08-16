% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_set_probability}
\alias{poltrx_set_probability}
\title{Set probability in a finite Polya tree}
\usage{
poltrx_set_probability(epsilon, tree)
}
\arguments{
\item{epsilon}{Binary string, an integer vector of 0s and 1s.}
\item{tree}{A tree as returned by \code{poltrx_finite_tree}.}
}
\description{
Accumulated product of branch probabilities down to \code{B_epsilon}.
}
\value{
A list with \code{probability}, \code{epsilon} and   \code{level}.
}
\keyword{internal}
