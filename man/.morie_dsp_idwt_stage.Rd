% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dsp_idwt_stage}
\alias{.morie_dsp_idwt_stage}
\title{Internal helper: inverse DWT stage (periodic, upsample by 2)}
\usage{
.morie_dsp_idwt_stage(W, V, g, h)
}
\description{
Exact adjoint of the analysis stage: the transform is orthonormal, so the inverse is the transpose of the analysis operator.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
.morie_dsp_idwt_stage(W = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8), g = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
