% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{han_forward}
\alias{han_forward}
\title{han_forward}
\usage{
han_forward(H, edges, types, metapaths, a_vec, W_node, W_sem, b_sem, q_sem, slope = 0.2)
}
\arguments{
\item{H}{A matrix; indexed by row and column.}
\item{edges}{See Usage.}
\item{types}{See Usage.}
\item{metapaths}{A vector; indexed elementwise.}
\item{a_vec}{See Usage.}
\item{W_node}{A matrix; passed to \code{nrow}.}
\item{W_sem}{See Usage.}
\item{b_sem}{See Usage.}
\item{q_sem}{See Usage.}
\item{slope}{Defaults to \code{0.2}.}
}
\description{
A step of the hetgnn_native implementation. Called by \code{morie_hetgnn}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{embeddings}, \code{semantic_weights}, \code{per_metapath}, \code{method}, \code{note}.
}
\keyword{internal}
