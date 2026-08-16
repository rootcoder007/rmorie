% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HomoFilt}
\alias{HomoFilt}
\title{Section 4.7.1, Figure 4.23: log -> linear filter -> exp.  The signal}
\usage{
HomoFilt(y, cutoff, keep = "low")
}
\arguments{
\item{y}{See Usage.}
\item{cutoff}{See Usage.}
\item{keep}{Defaults to \code{"low"}.}
}
\description{
must be strictly positive (eq 4.59\\'s side condition); a signal that crosses zero needs the complex-log route of HomDeconv.  Rejected rather than clipped, since clipping changes the factorization.
}
\value{
A list with \code{y}, \code{log_domain}, \code{log_input}, \code{cutoff}, \code{keep}, \code{n}, \code{stages}, \code{method}.
}
\keyword{internal}
\examples{
HomoFilt(y = c(1, 2, 3, 4, 5, 6, 7, 8), cutoff = 0.5)
}
