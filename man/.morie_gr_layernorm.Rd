% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_layernorm}
\alias{.morie_gr_layernorm}
\title{.morie_gr_layernorm}
\usage{
.morie_gr_layernorm(X, eps = 1e-05)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{.morie_gr_encoder_block}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_layernorm(V)
}
