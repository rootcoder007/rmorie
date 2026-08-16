% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_pesdol_ardl_bounds}
\alias{morie_pesdol_ardl_bounds}
\alias{morie_pesdol}
\title{ARDL conditional ECM and the Pesaran-Shin-Smith bounds}
\usage{
morie_pesdol_ardl_bounds(y, x, p = 1, q = 1)
}
\description{
R arm of pesdol -- ARDL conditional ECM and the Pesaran-Shin-Smith bounds Econometrics 16(3), 289-326. Mirrors src/morie/fn/pesdol.py.
}
\value{
A list with components \code{drop}, \code{estimate}, \code{long_run}, \code{coefficients}, \code{residuals}, \code{fitted}, \code{speed_of_adjustment}, \code{f_statistic}, \code{bound_lower}, \code{bound_upper}, \code{verdict}, \code{n_used}, \code{n_params}, \code{rss_unrestricted}, \code{rss_restricted}, \code{p}, \code{q}, \code{n_regressors}, \code{method}, \code{note}.
}
\references{
test. Pesaran, M. H. & Shin, Y. (1998), Ch. 11 in Econometrics and Economic
Theory in the 20th Century; Pesaran, Shin & Smith (2001) J. Appl.
}
\keyword{internal}
