% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cgnonl_beta}
\alias{.cgnonl_beta}
\title{.cgnonl_beta}
\usage{
.cgnonl_beta(rule, g_new, g_old)
}
\arguments{
\item{rule}{One of \code{"fletcher-reeves"}, \code{"polak-ribiere"}, \code{"polak-ribiere-plus"}.}
\item{g_new}{See Usage.}
\item{g_old}{See Usage.}
}
\description{
A step of the cgnonl_native implementation. Called by \code{nonlinear_cg}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
