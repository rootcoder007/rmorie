% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_fcnn_predict}
\alias{morie_propinf_fcnn_predict}
\title{Predict with a fully connected binary classifier}
\usage{
morie_propinf_fcnn_predict(net, X) 
}
\arguments{
\item{net}{Network as returned by \code{morie_propinf_train_fcnn}.}
\item{X}{Numeric matrix of inputs.}
}
\description{
Output of the network on each row of \code{X}.
}
\value{
Numeric vector of predicted probabilities.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(60), 20, 3)
y <- as.integer(X[, 1] + rnorm(20, 0, 0.3) > 0)
net <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 5L,
                                seed = 1L)
morie_propinf_fcnn_predict(net, X[1:3, , drop = FALSE])
}
