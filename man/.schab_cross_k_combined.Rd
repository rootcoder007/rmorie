% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_cross_k_combined}
\alias{.schab_cross_k_combined}
\title{.schab_cross_k_combined}
\usage{
.schab_cross_k_combined(p1, p2, region, r, correction = "ripley")
}
\arguments{
\item{p1}{A matrix; passed to \code{nrow}.}
\item{p2}{A matrix; passed to \code{nrow}.}
\item{region}{A vector; indexed elementwise.}
\item{r}{Numeric; combined arithmetically in the body.}
\item{correction}{Passed to \code{.schab_cross_k}. Defaults to \code{"ripley"}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spkcrs}. See the file header for the source the module follows.
}
\value{
A list with \code{K_star}, \code{K_12}, \code{K_21}, \code{L_star}, \code{L_minus_h}, \code{K_independence}, \code{r}, \code{lambda_1}, \code{lambda_2}.
}
\keyword{internal}
