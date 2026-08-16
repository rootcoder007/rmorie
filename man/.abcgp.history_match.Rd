% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.history_match}
\alias{.abcgp.history_match}
\title{.abcgp.history_match}
\usage{
.abcgp.history_match(sim, obs, prior_ppf, n_waves = 3, n_design = 32, n_sim = 50, epsilon = 1, summary = NULL, threshold = 10, n_sd = 3, kernel = "sqexp", accept_kernel = "gaussian", seed = 0)
}
\arguments{
\item{sim}{See Usage.}
\item{obs}{See Usage.}
\item{prior_ppf}{See Usage.}
\item{n_waves}{Defaults to \code{3}.}
\item{n_design}{Defaults to \code{32}.}
\item{n_sim}{Defaults to \code{50}.}
\item{epsilon}{Defaults to \code{1}.}
\item{summary}{Defaults to \code{NULL}.}
\item{threshold}{Defaults to \code{10}.}
\item{n_sd}{Defaults to \code{3}.}
\item{kernel}{Defaults to \code{"sqexp"}.}
\item{accept_kernel}{Defaults to \code{"gaussian"}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the abcgp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{fit}, \code{waves}.
}
\keyword{internal}
