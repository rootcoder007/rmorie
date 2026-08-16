% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_flat_representation}
\alias{morie_propinf_flat_representation}
\title{Baseline feature vector: every weight and bias in one vector}
\usage{
morie_propinf_flat_representation(net)
}
\arguments{
\item{net}{Network as returned by \code{morie_propinf_train_fcnn}.}
}
\description{
Flattening all the weights and biases into one vector. Kept because it is the thing the other two have to beat, not because it works.
}
\value{
Numeric vector.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(60), 20, 3)
y <- as.integer(X[, 1] + rnorm(20, 0, 0.3) > 0)
net <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 5L,
                                seed = 1L)
length(morie_propinf_flat_representation(net))
}
