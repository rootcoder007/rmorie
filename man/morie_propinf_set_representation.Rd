% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_set_representation}
\alias{morie_propinf_set_representation}
\title{Set feature representation: each layer as a set of (weights, bias)}
\usage{
morie_propinf_set_representation(net) 
}
\arguments{
\item{net}{Network as returned by \code{morie_propinf_train_fcnn}.}
}
\description{
Algorithm 2\'s input to the DeepSets meta-classifier.
}
\value{
List of layers, each a list of \code{c(weights, bias)}   numeric vectors.
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
# Algorithm 2: each layer as a set of (weights, bias) vectors
morie_propinf_set_representation(net)
}
