% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hetgnn}
\alias{morie_hetgnn}
\title{morie_hetgnn}
\usage{
morie_hetgnn(H, edges, types, metapaths, a_vec, W_node, W_sem, b_sem, q_sem, slope = 0.2)
}
\arguments{
\item{H}{Passed to \code{han_forward}.}
\item{edges}{Passed to \code{han_forward}.}
\item{types}{Passed to \code{han_forward}.}
\item{metapaths}{Passed to \code{han_forward}.}
\item{a_vec}{Passed to \code{han_forward}.}
\item{W_node}{Passed to \code{han_forward}.}
\item{W_sem}{Passed to \code{han_forward}.}
\item{b_sem}{Passed to \code{han_forward}.}
\item{q_sem}{Passed to \code{han_forward}.}
\item{slope}{Passed to \code{han_forward}. Defaults to \code{0.2}.}
}
\description{
A step of the hetgnn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{han_forward}.
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
r <- morie_hetgnn(H, edges, types, metapaths, a_vec, W_node,
                  W_sem, b_sem, q_sem)
nrow(r$embeddings)
}
