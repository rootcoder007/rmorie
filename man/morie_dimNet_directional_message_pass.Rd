% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dimNet_directional_message_pass}
\alias{morie_dimNet_directional_message_pass}
\title{One round of directional message passing}
\usage{
morie_dimNet_directional_message_pass(messages, adj, R, interact, update, cutoff = 5, n_rbf = 8L, n_sbf = 4L) 
}
\arguments{
\item{messages}{Named list keyed by directed edges \code{(j, i)}.}
\item{adj}{Named list mapping each node to its neighbours.}
\item{R}{Numeric matrix of positions.}
\item{interact}{Function of \code{(message, rbf, sbf)} returning a   numeric vector.}
\item{update}{Function of \code{(message, aggregated)} returning a   numeric vector.}
\item{cutoff}{Numeric cutoff for the Bessel basis.}
\item{n_rbf}{Integer number of radial basis functions.}
\item{n_sbf}{Integer number of angular basis functions.}
}
\description{
One round of directional message passing
}
\value{
A list with the updated messages, the count and the   triplets.
}
\keyword{internal}
\examples{
morie_dimNet_directional_message_pass(messages = c(1, 2, 3, 4, 5, 6, 7, 8),
  adj = c(1, 2, 3, 4, 5, 6, 7, 8), R = c(1, 2, 3, 4, 5, 6, 7, 8),
  interact = c(1, 2, 3, 4, 5, 6, 7, 8), update = c(1, 2, 3, 4, 5, 6, 7, 8))
}
