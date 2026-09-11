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
# a small fully connected net: three hidden units over two inputs,
# then a single logistic output
net <- list(
  list(W = matrix(c(2, 3,
                    1, 0,
                    4, 5), nrow = 3, byrow = TRUE), b = c(10, 20, 30)),
  list(W = matrix(c(0.5, -0.5, 0.25), nrow = 1), b = 7)
)
X <- matrix(c(1, -2, 0.5, 3), nrow = 2, byrow = TRUE)
perm <- morie_propinf_permute_hidden_layer(net, 0L, c(2L, 0L, 1L))
# relabelling the units of a hidden layer changes nothing the net computes
all.equal(morie_propinf_fcnn_predict(net, X),
          morie_propinf_fcnn_predict(perm, X))
}
