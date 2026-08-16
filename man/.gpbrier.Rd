% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpbrier}
\alias{.gpbrier}
\title{.gpbrier}
\usage{
.gpbrier(probs, y_true, n_classes = NULL, halved = FALSE)
}
\arguments{
\item{probs}{Passed to \code{.gpmat}.}
\item{y_true}{Passed to \code{.gpflat}.}
\item{n_classes}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{halved}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm009}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
