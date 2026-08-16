% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_dc_d}
\alias{.schab_dc_d}
\title{.schab_dc_d}
\usage{
.schab_dc_d(p1, p2, region, r)
}
\arguments{
\item{p1}{Passed to \code{.schab_k_border}.}
\item{p2}{Passed to \code{.schab_k_border}.}
\item{region}{Passed to \code{.schab_k_border}.}
\item{r}{Passed to \code{.schab_k_border}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spkcrs}. See the file header for the source the module follows.
}
\value{
A list with \code{D}, \code{K_11}, \code{K_22}, \code{r}.
}
\keyword{internal}
\examples{
.schab_dc_d(p1 = c(1, 2, 3, 4, 5, 6, 7, 8), p2 = c(1, 2, 3, 4, 5, 6, 7, 8), region = c(1, 2, 3, 4, 5, 6, 7, 8), r = c(1, 2, 3, 4, 5, 6, 7, 8))
}
