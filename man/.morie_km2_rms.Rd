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
\item{x}{See Usage.}
\item{gamma}{Defaults to \code{NULL}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_rms_norm}. See the file header for the source the module follows.
}
\value{
A list with \code{tensor}, \code{rms}.
}
\keyword{internal}
