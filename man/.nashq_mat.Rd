% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nashq_mat}
\alias{.nashq_mat}
\title{.nashq_mat}
\usage{
.nashq_mat(M, name)
}
\arguments{
\item{M}{A matrix; passed to \code{nrow}.}
\item{name}{Passed to \code{sprintf}.}
}
\description{
A step of the nashq_native implementation. Called by \code{nash_equilibria_bimatrix}, \code{stage_game_type}. See the file header for the source the module follows.
}
\value{
The value of \code{M}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.nashq_mat(V, V)
}
