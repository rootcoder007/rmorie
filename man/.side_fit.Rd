% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.side_fit}
\alias{.side_fit}
\title{.side_fit}
\usage{
.side_fit(v, y, k_pt, bandwidth, order, side, kernel)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; indexed elementwise.}
\item{k_pt}{See Usage.}
\item{bandwidth}{See Usage.}
\item{order}{See Usage.}
\item{side}{One of \code{"left"}, \code{"right"}.}
\item{kernel}{Compared against \code{"triangular"}.}
}
\description{
A step of the bnskt2_native implementation. Called by \code{covariate_kink_test}, \code{density_kink_test}, \code{local_polynomial_slope} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{slope}, \code{coef}, \code{n}.
}
\keyword{internal}
