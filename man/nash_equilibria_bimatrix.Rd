% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nash_equilibria_bimatrix}
\alias{nash_equilibria_bimatrix}
\title{nash_equilibria_bimatrix}
\usage{
nash_equilibria_bimatrix(A, B, tol = 1e-09)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{B}{A matrix; indexed by row and column.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-09}.}
}
\description{
A step of the nashq_native implementation. Called by \code{.nashq_select}, \code{stage_game_type}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
nash_equilibria_bimatrix(V, V)
}
