% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_regression_mlp_pytorch}
\alias{morie_geron_regression_mlp_pytorch}
\title{Regression MLP as an nn.Sequential architecture (numpy/R-trained, no torch call) (Geron Ch 10, hmrgpt)}
\usage{
morie_geron_regression_mlp_pytorch(X, y, hidden = 8, epochs = 400, lr = 0.05, seed = 0) 
}
\arguments{
\item{X,y}{Data and targets.}
\item{hidden,epochs,lr,seed}{Passed to `morie_geron_regression_mlp`.}
}
\description{
Regression MLP as an nn.Sequential architecture (numpy/R-trained, no torch call) (Geron Ch 10, hmrgpt)
}
\value{
A list with \code{layers}, \code{sizes}, \code{n_parameters}, \code{predict}, \code{predictions}, \code{mse}, \code{loss_history}, \code{weights}, \code{biases}, \code{uses_torch}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_regression_mlp_pytorch(V, V)
}
