% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_periodogram}
\alias{.schab_periodogram}
\title{.schab_periodogram}
\usage{
.schab_periodogram(z, omit_zero_frequency = TRUE)
}
\arguments{
\item{z}{A matrix; passed to \code{nrow}.}
\item{omit_zero_frequency}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spperiod}. See the file header for the source the module follows.
}
\value{
A list with \code{periodogram}, \code{omega1}, \code{omega2}, \code{j}, \code{k}, \code{zero_index}, \code{nonzero_mask}, \code{mean_invariant}, \code{r}, \code{c}.
}
\keyword{internal}
