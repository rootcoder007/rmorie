% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_online_learning}
\alias{morie_geron_online_learning}
\title{Online (streaming) learning: sequential SGD, prequential loss (Geron Ch 1, hmonl)}
\usage{
morie_geron_online_learning(X_stream, y_stream, eta = 0.1, theta = NULL, decay = 0) 
}
\arguments{
\item{X_stream,y_stream}{Stream of rows/targets.}
\item{eta}{Base learning rate.}
\item{theta}{Optional start (default zeros).}
\item{decay}{Robbins-Monro decay.}
}
\description{
Online (streaming) learning: sequential SGD, prequential loss (Geron Ch 1, hmonl)
}
\value{
A list with \code{theta}, \code{trajectory}, \code{losses}, \code{cumulative_loss}, \code{mean_loss}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_online_learning(V, V)
}
