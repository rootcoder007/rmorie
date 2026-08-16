% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{infinitesimal_jackknife}
\alias{infinitesimal_jackknife}
\title{infinitesimal_jackknife}
\usage{
infinitesimal_jackknife(preds, in_bag, n, s, correction = TRUE)
}
\arguments{
\item{preds}{A vector; its length is taken.}
\item{in_bag}{See Usage.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{s}{Numeric; combined arithmetically in the body.}
\item{correction}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the hntfst_native implementation. Called by \code{honest_forest}. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
}
\keyword{internal}
