% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_rms}
\alias{.morie_km2_rms}
\title{Rmsnr\\'s core}
\usage{
.morie_km2_rms(x, gamma = NULL, eps = 1e-06)
}
\arguments{
\item{x}{A matrix; the body checks with \code{is.matrix}.}
\item{gamma}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_rms_norm}. See the file header for the source the module follows.
}
\value{
A list with \code{tensor}, \code{rms}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_km2_rms(V)
}
