% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_permute_hidden_layer}
\alias{morie_propinf_permute_hidden_layer}
\title{Reorder the neurons of a hidden layer}
\usage{
morie_propinf_permute_hidden_layer(net, t, sigma)
}
\arguments{
\item{net}{Network as returned by \code{morie_propinf_train_fcnn}.}
\item{t}{Index of a hidden layer.}
\item{sigma}{Permutation of \code{seq_len(nrow(W[[t]]))}.}
}
\description{
Section 5 of the paper: layer \code{t} becomes sigma(h_t) and the next layer\'s weight columns are permuted to match, so the network computes exactly the same function (Proposition 5.1).
}
\value{
A permuted copy of the network.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(60), 20, 3)
y <- as.integer(X[, 1] > 0)
net <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 5L,
                                seed = 1L)
p1 <- morie_propinf_fcnn_predict(net, X[1:3, , drop = FALSE])
net2 <- morie_propinf_permute_hidden_layer(net, t = 0,
                                           sigma = c(1L, 0L, 3L, 2L))
p2 <- morie_propinf_fcnn_predict(net2, X[1:3, , drop = FALSE])
all.equal(p1, p2)
}
