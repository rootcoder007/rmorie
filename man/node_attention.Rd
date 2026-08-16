% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{node_attention}
\alias{node_attention}
\title{node_attention}
\usage{
node_attention(h_i, neighbours, H, a_vec, W, slope = 0.2)
}
\arguments{
\item{h_i}{See Usage.}
\item{neighbours}{See Usage.}
\item{H}{See Usage.}
\item{a_vec}{See Usage.}
\item{W}{See Usage.}
\item{slope}{Defaults to \code{0.2}.}
}
\description{
Part of the hetgnn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{embedding}, \code{alpha}, \code{neighbours}.
}
\keyword{internal}
