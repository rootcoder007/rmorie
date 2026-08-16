% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gauss_newton}
\alias{.schab_gauss_newton}
\title{.schab_gauss_newton}
\usage{
.schab_gauss_newton(lags, ghat, counts, start, model = "exponential", kind = "wls", max_iter = 200L, tol = 1e-12, max_halvings = 40L)
}
\arguments{
\item{lags}{A vector; indexed elementwise.}
\item{ghat}{A vector; indexed elementwise.}
\item{counts}{A vector; indexed elementwise.}
\item{start}{Passed to \code{.schab_gn_project}.}
\item{model}{Passed to \code{.sp_semivariogram}. Defaults to \code{"exponential"}.}
\item{kind}{Passed to \code{.schab_gn_weights}. Defaults to \code{"wls"}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-12}.}
\item{max_halvings}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{40L}.}
}
\description{
A step of the schab_gn_shared implementation. Called by \code{.schab_fit_semivariogram}. See the file header for the source the module follows.
}
\value{
A list with \code{theta}, \code{objective}, \code{converged}, \code{iterations}.
}
\keyword{internal}
\examples{
.schab_gauss_newton(lags = c(1, 2, 3, 4, 5, 6, 7, 8), ghat = c(1, 2, 3, 4, 5, 6, 7, 8), counts = c(1, 2, 3, 4, 5, 6, 7, 8), start = c(1, 2, 3, 4, 5, 6, 7, 8))
}
