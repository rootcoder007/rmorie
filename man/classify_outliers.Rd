% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{classify_outliers}
\alias{classify_outliers}
\title{classify_outliers}
\usage{
classify_outliers(sd, od, sd_cut, od_cut)
}
\arguments{
\item{sd}{A vector; its length is taken and its elements indexed.}
\item{od}{A vector; indexed elementwise.}
\item{sd_cut}{Passed to \code{>}.}
\item{od_cut}{Passed to \code{>}.}
}
\description{
A step of the robpca_native implementation. Called by \code{morie_robpca}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
