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
\item{h_i}{Passed to \code{proj}.}
\item{neighbours}{A vector; its length is taken and its elements indexed.}
\item{H}{A vector; indexed elementwise.}
\item{a_vec}{Numeric; combined arithmetically in the body.}
\item{W}{A matrix; passed to \code{\%*\%}.}
\item{slope}{Numeric; combined arithmetically in the body. Defaults to \code{0.2}.}
}
\description{
A step of the hetgnn_native implementation. Called by \code{han_forward}. See the file header for the source the module follows.
}
\value{
A list with \code{embedding}, \code{alpha}, \code{neighbours}.
}
\keyword{internal}
\examples{
set.seed(1)
d <- 3; hid <- 4
W <- matrix(rnorm(hid * d, 0, 0.3), hid, d)
a_vec <- rnorm(hid, 0, 0.3)
H <- list(`1` = rnorm(d), `2` = rnorm(d), `3` = rnorm(d))
r <- node_attention(H[["1"]], c("2", "3"), H, a_vec, W)
c(length(r$embedding), length(r$alpha))
}
