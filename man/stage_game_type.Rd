% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{stage_game_type}
\alias{stage_game_type}
\title{stage_game_type}
\usage{
stage_game_type(A, B, tol = 1e-09)
}
\arguments{
\item{A}{See Usage.}
\item{B}{See Usage.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
Part of the nashq_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{equilibria}, \code{n_equilibria}, \code{has_global_optimal}, \code{has_saddle}, \code{global_optimal}, \code{saddle}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
stage_game_type(V, V)
}
