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
\item{hval}{See Usage.}
\item{kval}{See Usage.}
\item{theta}{See Usage.}
\item{c}{See Usage.}
\item{p}{See Usage.}
\item{n_quad}{Defaults to \code{40L}.}
\item{rtol}{Defaults to \code{1e-10}.}
\item{max_panels}{Defaults to \code{20000L}.}
\item{quiet_runs}{Defaults to \code{4L}.}
}
\description{
Part of the schab_st_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{value}, \code{upper}, \code{last_rel}, \code{tail_bound}.
}
\keyword{internal}
