% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_decoder_parameters}
\alias{narm_decoder_parameters}
\title{narm_decoder_parameters}
\usage{
narm_decoder_parameters(n_items, hidden, emb_dim)
}
\arguments{
\item{n_items}{Coerced to integer by the body, with \code{as.integer}.}
\item{hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{emb_dim}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the narm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{fully_connected}, \code{bilinear}, \code{ratio}, \code{note}.
}
\keyword{internal}
\examples{
narm_decoder_parameters(n_items = c(1, 2, 3, 4, 5, 6, 7, 8), hidden = c(1, 2, 3, 4, 5, 6, 7, 8), emb_dim = c(1, 2, 3, 4, 5, 6, 7, 8))
}
