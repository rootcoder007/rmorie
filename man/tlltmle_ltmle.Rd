% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tlltmle_ltmle}
\alias{tlltmle_ltmle}
\title{tlltmle_ltmle}
\usage{
tlltmle_ltmle(Q_seq, H_seq, Y_seq) 
}
\arguments{
\item{Q_seq}{A vector; its length is taken and its elements indexed.}
\item{H_seq}{A vector; its length is taken and its elements indexed.}
\item{Y_seq}{A vector; indexed elementwise.}
}
\description{
A step of the tlltmle_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{epsilons}, \code{Q_star}, \code{T}, \code{method}.
}
\keyword{internal}
\examples{
tlltmle_ltmle(Q_seq = c(1, 2, 3, 4, 5, 6, 7, 8), H_seq = c(1, 2, 3, 4, 5, 6, 7, 8), Y_seq = c(1, 2, 3, 4, 5, 6, 7, 8))
}
