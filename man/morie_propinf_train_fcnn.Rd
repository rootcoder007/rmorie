% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_train_fcnn}
\alias{morie_propinf_train_fcnn}
\title{Train a fully connected binary classifier by minibatch SGD}
\usage{
morie_propinf_train_fcnn(X, y, hidden = c(8L, 4L), epochs = 40L, lr = 0.1, batch_size = 16L, seed = 0L)
}
\arguments{
\item{X}{Numeric matrix of inputs.}
\item{y}{Numeric vector of 0/1 labels.}
\item{hidden}{Integer vector giving the size of each hidden layer.}
\item{epochs}{Number of passes over the data.}
\item{lr}{Positive learning rate.}
\item{batch_size}{Minibatch size.}
\item{seed}{Seed for the shared generator.}
}
\description{
Cross-entropy loss, ReLU hidden units, logistic output, with a Fisher-Yates shuffle on the LCG every epoch (Section 4 of the paper: shadow classifiers are ordinary fully connected networks trained by the same procedure the victim was).
}
\value{
A list of layers, each \code{list(W=, b=)}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(60), 20, 3)
y <- as.integer(X[, 1] + rnorm(20, 0, 0.3) > 0)
net <- morie_propinf_train_fcnn(X, y, hidden = c(4L), epochs = 5L,
                                seed = 1L)
str(net, max.level = 1)
}
