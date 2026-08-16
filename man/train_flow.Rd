% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{train_flow}
\alias{train_flow}
\title{Train a MAF by central-difference SGD}
\usage{
train_flow(flow, D, epochs = 40L, lr = 0.01, seed = 0L, batch = NULL)
}
\description{
The paper backpropagates through autograd; we run central differences on the masked parameters so the two arms agree bit-for-bit at the cost of a few extra forward passes.
}
\value{
The value of \code{flow}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
train_flow(D, V)
}
