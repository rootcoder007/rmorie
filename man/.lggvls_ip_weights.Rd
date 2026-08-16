% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lggvls_ip_weights}
\alias{.lggvls_ip_weights}
\title{IPW for a single time point, binary treatment}
\usage{
.lggvls_ip_weights(a, den_X, num_X, kind = "binary", stabilize = TRUE)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{den_X}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{num_X}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{kind}{Compared against \code{"binary"}. Defaults to \code{"binary"}.}
\item{stabilize}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the lggvls_native implementation. Called by \code{morie_lggvls}. See the file header for the source the module follows.
}
\value{
A list with \code{w}, \code{info}.
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
