% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hibrid_reml_at}
\alias{.hibrid_reml_at}
\title{Restricted log likelihood at the two ratios, sigma_e^2 profiled out}
\usage{
.hibrid_reml_at(la, ls, Kg, Ks, y, X)
}
\arguments{
\item{la}{Numeric; combined arithmetically in the body.}
\item{ls}{Numeric; combined arithmetically in the body.}
\item{Kg}{Numeric; combined arithmetically in the body.}
\item{Ks}{Numeric; combined arithmetically in the body.}
\item{y}{A vector; its length is taken.}
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the hibrid_native implementation. Called by \code{morie_hibrid_hibrid_prediction}. See the file header for the source the module follows.
}
\value{
A list with \code{ll}, \code{beta}, \code{s2e}, \code{L}.
}
\keyword{internal}
