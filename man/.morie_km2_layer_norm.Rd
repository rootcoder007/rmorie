% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_layer_norm}
\alias{.morie_km2_layer_norm}
\title{.morie_km2_layer_norm}
\usage{
.morie_km2_layer_norm(x, eps = 1e-05)
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_layer_norm_rows}, \code{morie_kamath_post_ln_transformer}, \code{morie_kamath_pre_ln_transformer}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_km2_layer_norm(V)
}
