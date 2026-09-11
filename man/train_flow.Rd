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
\arguments{
\item{flow}{Argument \code{flow}; see Usage.}
\item{D}{Argument \code{D}; see Usage.}
\item{epochs}{Argument \code{epochs}; see Usage.}
\item{lr}{Argument \code{lr}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
\item{batch}{Argument \code{batch}; see Usage.}
}
\value{
The value of \code{flow}, as built in the body.
}
\keyword{internal}
\examples{
flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
set.seed(1)
D <- lapply(1:30, function(k) {
  th <- rnorm(2)
  list(th, th * 0.5 + rnorm(2, 0, 0.2))
})
ll <- function(fl) mean(vapply(D, function(p)
  flow_logprob(fl, p[[2]], p[[1]]), numeric(1)))
trained <- train_flow(flow, D, epochs = 10L, lr = 0.02, seed = 1L)
round(c(before = ll(flow), after = ll(trained)), 3)
}
