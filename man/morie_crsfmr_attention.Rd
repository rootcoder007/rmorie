% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_attention}
\alias{morie_crsfmr_attention}
\title{Scaled dot-product attention}
\usage{
morie_crsfmr_attention(Q, K, V)
}
\arguments{
\item{Q}{Numeric matrix (rows are queries).}
\item{K}{Numeric matrix (rows are keys).}
\item{V}{Numeric matrix (rows are values).}
}
\description{
Mirrors \code{attention}: \code{out = softmax(Q K^T / sqrt(d_k)) V} with the rows of the weight matrix summing to one.
}
\value{
A list with \code{out} and \code{weights}.
}
\keyword{internal}
\examples{
morie_crsfmr_attention(Q = 0.5, K = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
