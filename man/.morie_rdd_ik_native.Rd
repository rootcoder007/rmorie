% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_ik_native}
\alias{.morie_rdd_ik_native}
\title{Internal helper: Imbens-Kalyanaraman (2012) plug-in bandwidth}
\usage{
.morie_rdd_ik_native(x, y, cutoff = 0, kernel = "triangular")
}
\description{
The three-step algorithm of IK (2012, REStud 79): (1) pilot density and conditional variance at the cutoff; (2) second derivatives from one-sided quadratic fits at pilot bandwidths driven by a global cubic; (3) regularized MSE-optimal bandwidth with the kernel-specific constant (3.4375 for the triangular kernel).
}
\value{
A list with \code{bandwidth}, \code{method}, \code{details}.
}
\keyword{internal}
