% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sxrhrt_reml}
\alias{.sxrhrt_reml}
\title{Restricted log likelihood at (s2gm, s2gf, rg, s2em, s2ef)}
\usage{
.sxrhrt_reml(theta, y, X, Km, male)
}
\arguments{
\item{theta}{A vector; indexed elementwise.}
\item{y}{A vector; its length is taken.}
\item{X}{A matrix; indexed by row and column.}
\item{Km}{A matrix; indexed by row and column.}
\item{male}{A vector; indexed elementwise.}
}
\description{
A step of the sxrhrt_native implementation. Called by \code{morie_sxrhrt_sex_specific_h2}. See the file header for the source the module follows.
}
\value{
A list with \code{ll}, \code{beta}, \code{L}.
}
\keyword{internal}
\examples{
.sxrhrt_reml(theta = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), Km = c(1, 2, 3, 4, 5, 6, 7, 8), male = c(1, 2, 3, 4, 5, 6, 7, 8))
}
