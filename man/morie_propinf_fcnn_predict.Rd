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
# a small fully connected net: three hidden units over two inputs,
# then a single logistic output
net <- list(
  list(W = matrix(c(2, 3,
                    1, 0,
                    4, 5), nrow = 3, byrow = TRUE), b = c(10, 20, 30)),
  list(W = matrix(c(0.5, -0.5, 0.25), nrow = 1), b = 7)
)
X <- matrix(c(1, -2, 0.5, 3), nrow = 2, byrow = TRUE)
morie_propinf_fcnn_predict(net, X)
}
