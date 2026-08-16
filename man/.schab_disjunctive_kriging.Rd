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
\item{correlation_fn}{Accepted by the signature and not used anywhere in the body.}
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
\examples{
dj_x <- c(0, 0.5, 1, -1.3, 2.7)
h <- .schab_hermite_e(dj_x, 4)
b <- .schab_hermite_coefficients(function(v) v, 6)
g <- 0:4
C <- as.matrix(expand.grid(x = g, y = g))
y <- sin(C[, 1] * 0.6) + cos(C[, 2] * 0.4)
.schab_disjunctive_kriging(C, y, c(2.3, 1.7), function(h) exp(-h/3), 
    b, 6)
}
