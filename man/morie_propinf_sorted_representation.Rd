% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_sorted_representation}
\alias{morie_propinf_sorted_representation}
\title{Sorted feature vector: canonicalise then flatten (Algorithm 1)}
\usage{
morie_propinf_sorted_representation(net, metric = NULL) 
}
\arguments{
\item{net}{Network as returned by \code{morie_propinf_train_fcnn}.}
\item{metric}{Optional function \code{(layer, i)} returning a   scalar. Defaults to the magnitude of the node\'s weight sum.}
}
\description{
Hidden layers are sorted by \code{metric} (default: the magnitude of the sum of the node\'s weights) in descending order; the output layer cannot be permuted and is appended as it stands.
}
\value{
Numeric vector.
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
# Algorithm 1: units sorted by |sum of weights|, here 5, 1, 9, so the
# canonical order of the rows is 3, 1, 2
morie_propinf_sorted_representation(net)
perm <- morie_propinf_permute_hidden_layer(net, 0L, c(2L, 0L, 1L))
identical(morie_propinf_sorted_representation(net),
          morie_propinf_sorted_representation(perm))
}
