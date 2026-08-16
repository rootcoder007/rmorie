% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfcand}
\alias{.rfcand}
\title{.rfcand}
\usage{
.rfcand(b, s, p, mtry)
}
\arguments{
\item{b}{Numeric; combined arithmetically in the body.}
\item{s}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{mtry}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{.rfgrow}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
