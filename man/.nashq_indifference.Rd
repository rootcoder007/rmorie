% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nashq_indifference}
\alias{.nashq_indifference}
\title{Unknowns x_0..x_{k-1}, v}
\usage{
.nashq_indifference(payoff, k)
}
\arguments{
\item{payoff}{A matrix; indexed by row and column.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the nashq_native implementation. Called by \code{nash_equilibria_bimatrix}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
