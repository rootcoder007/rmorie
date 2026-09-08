% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{check_network_assumption}
\alias{check_network_assumption}
\title{The conditions the identification rests on}
\usage{
check_network_assumption(friends, N = NULL) 
}
\description{
Degrees must vanish relative to \eqn{N}, and the network must be symmetric -- an asymmetric "friend" relation means influence flows somewhere the model does not represent.
}
\arguments{
\item{friends}{Argument \code{friends}; see Usage.}
\item{N}{Argument \code{N}; see Usage.}
}
\value{
A list with \code{max_degree}, \code{max_share}, \code{sparse}, \code{asymmetric_edges}, \code{symmetric}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::check_network_assumption(V)
}
