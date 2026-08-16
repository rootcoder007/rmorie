% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rl_sample_row}
\alias{.mor_rl_sample_row}
\title{Inverse-CDF draw on a stochastic row from one uniform (Python}
\usage{
.mor_rl_sample_row(row, S, u)
}
\arguments{
\item{row}{A vector; indexed elementwise.}
\item{S}{A count; the body uses it as \code{seq_len(...)}.}
\item{u}{See Usage.}
}
\description{
_sample_row); returns a 1-based state index
}
\value{
The value of \code{S}, as built in the body.
}
\keyword{internal}
\examples{
.mor_rl_sample_row(row = c(1, 2, 3, 4, 5, 6, 7, 8), S = c(1, 2, 3, 4, 5, 6, 7, 8), u = 5L)
}
