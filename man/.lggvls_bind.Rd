% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lggvls_bind}
\alias{.lggvls_bind}
\title{Bind a list of length-n vectors into an n-by-p matrix}
\usage{
.lggvls_bind(cols, n)
}
\arguments{
\item{cols}{A vector; its length is taken.}
\item{n}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the lggvls_native implementation. Called by \code{morie_lggvls}. See the file header for the source the module follows.
}
\value{
The value of \code{do.call}.
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
