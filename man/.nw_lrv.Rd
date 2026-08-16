% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nw_lrv}
\alias{.nw_lrv}
\title{Internal: Newey-West (1987) long-run variance, default bandwidth}
\usage{
.nw_lrv(u, bandwidth = NULL)
}
\arguments{
\item{u}{A vector; its length is taken.}
\item{bandwidth}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
4 (T/100)^(2/9).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.nw_lrv(V)
}
