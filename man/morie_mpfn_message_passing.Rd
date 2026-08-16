% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mpfn_message_passing}
\alias{morie_mpfn_message_passing}
\title{T rounds of eq. (1)}
\usage{
morie_mpfn_message_passing(H0, adj, edge_features, T = 3L, A = NULL, update = NULL)
}
\arguments{
\item{H0}{Initial states (n x d matrix).}
\item{adj}{Adjacency list (list of integer vectors).}
\item{edge_features}{Edge feature map.}
\item{T}{Number of steps.}
\item{A}{Optional edge network.}
\item{update}{Optional update function.}
}
\description{
T rounds of eq. (1)
}
\value{
Final states.
}
\keyword{internal}
