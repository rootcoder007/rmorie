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
set.seed(1)
X <- matrix(rnorm(60), 20, 3)
y <- as.integer(X[, 1] + rnorm(20, 0, 0.3) > 0)
net <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 5L,
                                seed = 1L)
str(morie_propinf_set_representation(net), max.level = 1)
}
