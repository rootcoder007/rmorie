% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_W}
\alias{.fz_W}
\title{W(u) = int_{-inf}^u K(v) dv, the INTEGRATED kernel of (2.2). A}
\usage{
.fz_W(u)
}
\arguments{
\item{u}{See Usage.}
}
\description{
distribution-function estimator smooths with the kernel\\'s integral, not the kernel, which is what makes it continuous where the empirical df jumps and why its bias carries f\\' rather than f\\'\\'.
}
\value{
The value of \code{stats::pnorm}.
}
\keyword{internal}
