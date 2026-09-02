% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Shannon}
\alias{Shannon}
\title{Eq (3.11): H = - sum p(eta_l) log2 p(eta_l), over L quantized values}
\usage{
Shannon(p, levels = NULL) 
}
\arguments{
\item{p}{Passed to \code{.morie_rg_aslist}.}
\item{levels}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_ch3 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{entropy}, \code{units}, \code{levels}, \code{max_entropy}, \code{probabilities}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Shannon(V)
}
