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
set.seed(1)
X <- matrix(rnorm(60), 20, 3)
y <- as.integer(X[, 1] > 0)
net <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 5L,
                                seed = 1L)
length(morie_propinf_sorted_representation(net))
}
