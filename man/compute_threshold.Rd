% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{compute_threshold}
\alias{compute_threshold}
\title{compute_threshold}
\usage{
compute_threshold(i1, i2, a1_new, a2_new, alpha, y, E, K, b, C)
}
\arguments{
\item{i1}{Coerced to integer by the body, with \code{as.integer}.}
\item{i2}{Coerced to integer by the body, with \code{as.integer}.}
\item{a1_new}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{a2_new}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{E}{A vector; indexed elementwise.}
\item{K}{A matrix; indexed by row and column.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the smoopt_native implementation. Called by \code{smo_platt}. See the file header for the source the module follows.
}
\value{
A list with \code{b}, \code{from}, \code{b1}, \code{b2}, \code{note}.
}
\keyword{internal}
