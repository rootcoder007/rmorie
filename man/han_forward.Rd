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
\item{edges}{Passed to \code{metapath_neighbours}.}
\item{types}{Passed to \code{metapath_neighbours}.}
\item{metapaths}{A vector; indexed elementwise.}
\item{a_vec}{Passed to \code{node_attention}.}
\item{W_node}{A matrix; passed to \code{nrow}.}
\item{W_sem}{Passed to \code{semantic_attention}.}
\item{b_sem}{Passed to \code{semantic_attention}.}
\item{q_sem}{Passed to \code{semantic_attention}.}
\item{slope}{Passed to \code{node_attention}. Defaults to \code{0.2}.}
}
\description{
A step of the hetgnn_native implementation. Called by \code{morie_hetgnn}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{embeddings}, \code{semantic_weights}, \code{per_metapath}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
d <- 3; hid <- 4; sem <- 2
W_node <- matrix(rnorm(hid * d, 0, 0.3), hid, d)
a_vec <- rnorm(hid, 0, 0.3)
H <- matrix(rnorm(9), 3, 3)
edges <- list(`1` = c("3"), `2` = c("3"), `3` = c("1", "2"))
types <- list(`1` = "movie", `2` = "movie", `3` = "actor")
metapaths <- list(MAM = c("movie", "actor", "movie"))
W_sem <- matrix(rnorm(sem * hid, 0, 0.3), sem, hid)
b_sem <- rnorm(sem); q_sem <- rnorm(sem)
r <- han_forward(H, edges, types, metapaths, a_vec, W_node,
                 W_sem, b_sem, q_sem)
dim(r$embeddings)
}
