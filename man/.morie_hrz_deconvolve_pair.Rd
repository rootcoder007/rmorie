% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_hrz_deconvolve_pair}
\alias{.morie_hrz_deconvolve_pair}
\title{.morie_hrz_deconvolve_pair}
\usage{
.morie_hrz_deconvolve_pair(w, eta, grid_u, grid_z, nu_U, nu_eps, n_tau = 2001L)
}
\arguments{
\item{w}{See Usage.}
\item{eta}{See Usage.}
\item{grid_u}{See Usage.}
\item{grid_z}{See Usage.}
\item{nu_U}{Numeric; combined arithmetically in the body.}
\item{nu_eps}{Numeric; combined arithmetically in the body.}
\item{n_tau}{Defaults to \code{2001L}.}
}
\description{
A step of the horowitz_native7 implementation. Called by \code{morie_panel_deconvolution}, \code{morie_panel_densities}, \code{morie_smoothed_fU}. See the file header for the source the module follows.
}
\value{
A list with \code{f_U}, \code{f_eps}.
}
\keyword{internal}
