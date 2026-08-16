% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_point_source_corr}
\alias{.schab_point_source_corr}
\title{.schab_point_source_corr}
\usage{
.schab_point_source_corr(coords, source, theta1, theta2 = 0, theta3 = 0, anisotropy = NULL, source_anisotropy = NULL)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{source}{See Usage.}
\item{theta1}{Numeric; combined arithmetically in the body.}
\item{theta2}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{theta3}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{anisotropy}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{source_anisotropy}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spnst}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
