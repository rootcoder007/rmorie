% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{precision_recall}
\alias{precision_recall}
\title{precision_recall}
\usage{
precision_recall(pred, truth)
}
\arguments{
\item{pred}{Coerced to integer by the body, with \code{as.integer}.}
\item{truth}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the memb_native implementation. Called by \code{memb}, \code{morie_memb}. See the file header for the source the module follows.
}
\value{
A list with \code{precision}, \code{recall}, \code{accuracy}, \code{tp}, \code{fp}, \code{fn}, \code{tn}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
precision_recall(V, V)
}
