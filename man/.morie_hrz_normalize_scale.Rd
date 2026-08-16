% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hrz_normalize_scale}
\alias{.morie_hrz_normalize_scale}
\title{.morie_hrz_normalize_scale}
\usage{
.morie_hrz_normalize_scale(beta)
}
\arguments{
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the horowitz_native6 implementation. Called by \code{morie_chen_transform}, \code{morie_transform_prediction}, \code{morie_transform_T_F}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_hrz_normalize_scale(V)
}
