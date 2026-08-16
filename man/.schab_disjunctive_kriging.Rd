% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_disjunctive_kriging}
\alias{.schab_disjunctive_kriging}
\title{.schab_disjunctive_kriging}
\usage{
.schab_disjunctive_kriging(coords, y, target, correlation_fn, b, degree)
}
\arguments{
\item{coords}{A matrix; passed to \code{nrow}.}
\item{y}{A vector; its length is taken.}
\item{target}{A count; the body uses it as \code{matrix(...)}.}
\item{correlation_fn}{See Usage.}
\item{b}{A vector; indexed elementwise.}
\item{degree}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the schab_hermite_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{prediction}, \code{variance}, \code{coefficients}, \code{component_variances}.
}
\keyword{internal}
