% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lggvls_as_history}
\alias{.lggvls_as_history}
\title{Wrap a single vector/matrix as a one-element history}
\usage{
.lggvls_as_history(obj, allow_none = FALSE)
}
\arguments{
\item{obj}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{allow_none}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the lggvls_native implementation. Called by \code{morie_lggvls}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\references{
----------
Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period -- application to control of
the healthy worker survivor effect", Mathematical Modelling 7(9-12),
1393-1512, doi:10.1016/0270-0255(86)90088-6.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC, Sec. 21.2 for the product-over-time weights
and Ch. 20 for why conditioning fails where weighting works.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.lggvls_as_history(V)
}
