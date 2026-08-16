% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_hankel_panels}
\alias{.schab_st_hankel_panels}
\title{.schab_st_hankel_panels}
\usage{
.schab_st_hankel_panels(hval, kval, theta, c, p, n_quad = 40L, rtol = 1e-10, max_panels = 20000L, quiet_runs = 4L)
}
\arguments{
\item{hval}{Numeric; combined arithmetically in the body.}
\item{kval}{Numeric; combined arithmetically in the body.}
\item{theta}{Numeric; combined arithmetically in the body.}
\item{c}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{n_quad}{Passed to \code{.schab_gauss_legendre}. Defaults to \code{40L}.}
\item{rtol}{Defaults to \code{1e-10}.}
\item{max_panels}{Defaults to \code{20000L}.}
\item{quiet_runs}{Defaults to \code{4L}.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{.schab_st_jones_zhang}. See the file header for the source the module follows.
}
\value{
A list with \code{value}, \code{upper}, \code{last_rel}, \code{tail_bound}.
}
\keyword{internal}
