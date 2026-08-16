% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_bits_f32}
\alias{.morie_gr_bits_f32}
\title{.morie_gr_bits_f32}
\usage{
.morie_gr_bits_f32(u)
}
\arguments{
\item{u}{A vector; its length is taken.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{.morie_gr_f32}, \code{morie_geron_bf16}, \code{morie_geron_bf16_range}. See the file header for the source the module follows.
}
\value{
The value of \code{readBin}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_bits_f32(V)
}
