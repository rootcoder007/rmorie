% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_fake_quant}
\alias{.morie_w4c_fake_quant}
\title{.morie_w4c_fake_quant}
\usage{
.morie_w4c_fake_quant(w, bits)
}
\arguments{
\item{w}{Numeric; passed to \code{abs}.}
\item{bits}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_quantization_aware_training_hm}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
.morie_w4c_fake_quant(w = c(1, 2, 3, 4, 5, 6, 7, 8), bits = 5L)
}
