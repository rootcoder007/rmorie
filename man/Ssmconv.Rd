% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ssmconv}
\alias{Ssmconv}
\title{Causal convolution y_t = sum_l K_l x_{t-l} (standard)}
\usage{
Ssmconv(K, x) 
}
\description{
Causal convolution y_t = sum_l K_l x_\{t-l\} (standard)
}
\arguments{
\item{K}{Argument \code{K}; see Usage.}
\item{x}{Argument \code{x}; see Usage.}
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ssmconv(V, V)
}
