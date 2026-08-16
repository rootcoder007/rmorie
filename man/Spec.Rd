% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Spec}
\alias{Spec}
\title{Eq (10.101): S- = TN / (subjects without the disease)}
\usage{
Spec(tn, fp = NULL)
}
\arguments{
\item{tn}{A matrix; passed to \code{as.matrix}.}
\item{fp}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_class implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{specificity}, \code{tnf}, \code{fpf}, \code{n_healthy}, \code{tn}, \code{fp}, \code{method}.
}
\keyword{internal}
