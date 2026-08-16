% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_V}
\alias{.fz_V}
\title{V(u) = 1 - W(u), the survival counterpart used throughout Ch. 4}
\usage{
.fz_V(u)
}
\arguments{
\item{u}{See Usage.}
}
\description{
A step of the fauzi_native implementation. Called by \code{morie_fauzi_cumulative_survival_1}, \code{morie_fauzi_cumulative_survival_2}, \code{morie_fauzi_mrl_naive}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.fz_V(V)
}
