% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_corr}
\alias{.morie_qrs_corr}
\title{.morie_qrs_corr}
\usage{
.morie_qrs_corr(a, b)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{EcgEmgCpl}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_qrs_corr(V, V)
}
