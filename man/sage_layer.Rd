% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sage_layer}
\alias{sage_layer}
\title{One depth of Algorithm 1}
\usage{
sage_layer(H, adj, W, how = "mean", sizes = NULL, rng = NULL, normalize = TRUE)
}
\arguments{
\item{H}{Node feature matrix (n x d).}
\item{adj}{Adjacency list keyed by character "0..n-1".}
\item{W}{Weight matrix.}
\item{how}{Aggregator.}
\item{sizes}{Optional sample size per node.}
\item{rng}{Optional generator environment.}
\item{normalize}{If TRUE, L2-normalise the output.}
}
\description{
One depth of Algorithm 1
}
\value{
Updated node feature matrix.
}
\keyword{internal}
