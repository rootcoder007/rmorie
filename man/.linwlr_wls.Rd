% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.linwlr_wls}
\alias{.linwlr_wls}
\title{.linwlr_wls}
\usage{
.linwlr_wls(X, y, w)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{w}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the linwlr_native implementation. Called by \code{morie_linwlr}. See the file header for the source the module follows.
}
\value{
A list with \code{coef}, \code{se}, \code{resid}.
}
\keyword{internal}
\examples{
.linwlr_wls(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
