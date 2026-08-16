% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_haas_window}
\alias{.schab_haas_window}
\title{.schab_haas_window}
\usage{
.schab_haas_window(coords, target, min_sites = 35L, step = 5L, lag_classes = NULL, max_sites = NULL)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{target}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{min_sites}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{35L}.}
\item{step}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{lag_classes}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{max_sites}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{.schab_moving_window_krige}. See the file header for the source the module follows.
}
\value{
The value of \code{repeat}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_haas_window(V, V)
}
