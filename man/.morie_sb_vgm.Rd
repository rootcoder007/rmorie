% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sb_vgm}
\alias{.morie_sb_vgm}
\title{.morie_sb_vgm}
\usage{
.morie_sb_vgm(h, model, nugget, psill, rng)
}
\arguments{
\item{h}{Numeric; combined arithmetically in the body.}
\item{model}{Passed to \code{switch}.}
\item{nugget}{Numeric; combined arithmetically in the body.}
\item{psill}{Numeric; combined arithmetically in the body.}
\item{rng}{Numeric; passed to \code{max}.}
}
\description{
A step of the schaben_native implementation. Called by \code{morie_kriging_pred_error}, \code{morie_variogram_composite_likelihood}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
